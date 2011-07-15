package com.yorrick.tasks.model

import scala.None
import java.lang.{ IllegalArgumentException, IllegalStateException }
import net.liftweb.http._
import net.liftweb.common.{ Logger, Failure, Box, Full, Empty }
import net.liftweb.mapper._

case object TaskImportance extends Enumeration {
  val Low, Normal, Important = Value
}


class Task extends LongKeyedMapper[Task] with IdPK {
  
  def getSingleton = Task
  
  object label extends MappedString(this, 50)
  object detail extends MappedString(this, 300)
  object importance extends MappedEnum[Task, TaskImportance.type](this, TaskImportance)
  
  def image : Option[Image] = Image.findById(this.id)
}


object Task extends Task with LongKeyedMetaMapper[Task] {
  override def dbTableName = "TASK"
  override def fieldOrder = List(label, detail, importance)
  
  def getTask(id : Int) : Task = Task.findAll(By(Task.id, id)) match {
    case task :: Nil => task
    case Nil => throw new IllegalArgumentException("No task with id " + id)
    case List(task1, task2, _*) => throw new IllegalStateException("More than one task with id " + id) 
  }
  
  def getTasks : List[Task] = getTasks(TaskImportance.Low)
  
  /** Fonction qui determine l'ordre dans lequel les taches sont renvoyees */
  val taskSorter = (task: Task) => (task.importance.is, task.id.is)
  
  /**
   * TODO simplifier, faire le tri côté base 
   */
  def getTasks(importance: TaskImportance.Value) : List[Task] =
    (Task.findAll(By_>(Task.importance, importance)) ::: Task.findAll(By(Task.importance, importance))) sortBy taskSorter reverse
    
}

class Image extends LongKeyedMapper[Image] with IdPK {
  
  def getSingleton = Image
  
  object data extends MappedBinary(this)
  object mimeType extends MappedString(this, 100)
  object task extends MappedLongForeignKey(this, Task)
  
}

object Image extends Image with LongKeyedMetaMapper[Image] {
  // le mime type n'apparait pas dans le mapping
  override def fieldOrder = List(data)
  override def dbTableName = "IMAGE"
    
  def findById(id : Long) : Option[Image] = findAll(By(Image.id, id)) match {
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
