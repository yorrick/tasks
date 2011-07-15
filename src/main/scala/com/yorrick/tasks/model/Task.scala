package com.yorrick.tasks.model

import scala.None
import java.lang.{ IllegalArgumentException, IllegalStateException }
import net.liftweb.http._
import net.liftweb.common.{ Logger, Failure, Box, Full, Empty }
import net.liftweb.mapper._

case object TaskImportance extends Enumeration {
  val Low, Normal, Important = Value
}


class Task2 extends LongKeyedMapper[Task2] with IdPK {
  
  def getSingleton = Task2
  
  object label extends MappedString(this, 50)
  object detail extends MappedString(this, 300)
  object importance extends MappedEnum[Task2, TaskImportance.type](this, TaskImportance)
  
  def image : Option[Image2] = Image2.findById(this.id)
}


object Task2 extends Task2 with LongKeyedMetaMapper[Task2] {
  override def dbTableName = "TASK"
  override def fieldOrder = List(label, detail, importance)
  
  def getTask(id : Int) : Task2 = Task2.findAll(By(Task2.id, id)) match {
    case task :: Nil => task
    case Nil => throw new IllegalArgumentException("No task with id " + id)
    case List(task1, task2, _*) => throw new IllegalStateException("More than one task with id " + id) 
  }
  
  def getTasks : List[Task2] = getTasks(TaskImportance.Low)
  
  /** Fonction qui determine l'ordre dans lequel les taches sont renvoyees */
  val taskSorter = (task: Task2) => (task.importance.is, task.id.is)
  
  /**
   * TODO simplifier, faire le tri côté base 
   */
  def getTasks(importance: TaskImportance.Value) : List[Task2] =
    (Task2.findAll(By_>(Task2.importance, importance)) ::: Task2.findAll(By(Task2.importance, importance))) sortBy taskSorter reverse
    
}

class Image2 extends LongKeyedMapper[Image2] with IdPK {
  
  def getSingleton = Image2
  
  object data extends MappedBinary(this)
  object mimeType extends MappedString(this, 100)
  object task extends MappedLongForeignKey(this, Task2)
  
}

object Image2 extends Image2 with LongKeyedMetaMapper[Image2] {
  // le mime type n'apparait pas dans le mapping
  override def fieldOrder = List(data)
  override def dbTableName = "IMAGE"
    
  def findById(id : Long) : Option[Image2] = findAll(By(Image2.id, id)) match {
    case Nil => Empty
    case head :: Nil => Full(head)
    case list => throw new IllegalStateException("There can not be more than one image per task")
  }
  
  /**
   * Retourne l'image identifiée par l'id
   */
  def viewImage(id: String): Box[LiftResponse] =
    try {
      findById(id.toLong) match {
        case None => Failure("No such image")
        case Some(image) => Full(InMemoryResponse(image.data.is, List("Content-Type" -> image.mimeType.is), Nil, 200))
      }
    } catch {
      case nfe: NumberFormatException => Failure("Invalid task ID")
    }  
}
