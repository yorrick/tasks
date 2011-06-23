package com.yorrick.tasks.model

import scala.None
import java.lang.{IllegalArgumentException, IllegalStateException}
import net.liftweb.http._
import net.liftweb.common.{Logger, Failure, Box, Full}

case object TaskImportance extends Enumeration {
  val Low, Normal, Important = Value
}

case class Image (val data : Array[Byte], val mimeType : String)

object Image {

  def viewImage(id: String): Box[LiftResponse] =
    try {
      Task.getTask(id.toInt).image match {
        case Some(image) => Full(InMemoryResponse(image.data, List("Content-Type" -> image.mimeType), Nil, 200))
        case None => Failure("No such task item")
      }
    } catch {
      case nfe: NumberFormatException => Failure("Invalid task ID")
    }

}

class Task(
  val id : Int,
  val label : String,
  val detail : String,
  val importance : TaskImportance.Value,
  val image : Option[Image] = None) {

  override def toString = "Task " + id + ", label : " + label
}

object Task {

  val log = Logger(classOf[Task])

  def apply(id : Int) = new Task(id, "", "", TaskImportance.Normal)

  private var tasks : List[Task] = List(
    new Task(1, "Dishes", "Boring, but I have to do it", TaskImportance.Low),
    new Task(2, "Tondeuse", "Passer la tondeuse", TaskImportance.Important),
    new Task(3, "Musique", "Enregistrer musique", TaskImportance.Important),
    new Task(4, "Banque", "Prendre RDV", TaskImportance.Normal),
    new Task(5, "DRH", "Demander les papiers", TaskImportance.Low)
  )

  def getTask(id : Int) : Task = tasks.filter(_.id == id) match {
    case task :: Nil => task
    case Nil => throw new IllegalArgumentException("No task with id " + id)
    case List(task1, task2, _*) => throw new IllegalStateException("More than one task with id " + id)
  }

  /**
   * Simule la persistence d'une tache
   */
  def saveTask(newTask : Task) : Task = {
    log.warn("saveTask" + newTask)

    if (newTask.id >= 0) {
      tasks.find(_.id == newTask.id) match {
        // cas de la modification
        case Some(taskToBeModified) =>
          tasks = newTask :: tasks.filterNot {taskToBeModified => taskToBeModified.id == newTask.id}
       // erreur : on renseigne un id qui n'existe pas
        case None =>
          throw new IllegalArgumentException("Not task with id " + newTask.id)
      }
      newTask
    }
    else {
      val nextId = tasks match {
        case Nil => 0
        case _ => (tasks max Ordering.by {t : Task => t.id}).id + 1
      }

      val createdTask = new Task(nextId, newTask.label, newTask.detail, newTask.importance, newTask.image)
      tasks = createdTask :: tasks
      createdTask
    }
  }

  /**
   * Simule la suppresion d'une tache
   */
  def removeTask(taskId : Int) : Task = {
    val removedTask = getTask(taskId)
    tasks = tasks.filterNot {taskToBeModified => taskToBeModified.id == taskId}
    removedTask
  }

  /** Fonction qui determine l'ordre dans lequel les taches sont renvoyees */
  val taskSorter = (task : Task) => (task.importance, task.id)

  /**
   * Simule un chargement de données
   */
  def getTasks(importance: TaskImportance.Value) : List[Task] =
    tasks filter (_.importance >= importance) sortBy taskSorter reverse

  /**
   * Retourne toutes les taches
   */
  def getTasks : List[Task] = getTasks(TaskImportance.Low)

}
