package com.yorrick.tasks.snippet

import com.yorrick.tasks.model.Task
import requestvars.{currentTask, taskImportance}
import net.liftweb.http.{SHtml, DispatchSnippet}
import xml.NodeSeq._
import xml.{Attribute, Null, NodeSeq, Text}
import net.liftweb.common.Full
import net.liftweb.util.BindHelpers._

object TasksListSnippet extends DispatchSnippet {

  def dispatch : DispatchIt = {
    case "viewTask" => viewTask _
  }

  def viewTask(content : NodeSeq) : NodeSeq = {
    val result : NodeSeq = Task.getTasks(taskImportance.is) match {
      case Nil => <span>Pas de taches</span>
      case list => list.flatMap(task => {
          val redirectPath = "/tasks/edition/"

          val imageTag = task.image match {
            case Some(image) =>
              val imageSource = "/tasks/image/" + task.id
              <img id="image" alt="Image de test"/> % Attribute(None, "src", Text(imageSource), Null)
            case None =>
              <span>Pas d'image pour cette tâche</span>
          }

          def removeTask = {
            task.image match {
              case Some(image) => image.delete_!
              case None => // rien a faire
            }
            task.delete_!
          }

          (
            "#label *"        #> (task.label + "(" + task.id + ", " + task.importance.toString + ")") &
            ".description *"  #> task.detail &
            "#image"          #> imageTag &
            "#editLink"       #> SHtml.link(redirectPath, () => currentTask(Full(task)), (content \\ "a")(0)) &
            "#removeLink"     #> SHtml.link("", removeTask _, (content \\ "a")(1))
          ).apply(content)
        }
      )
    }

    result
  }

}