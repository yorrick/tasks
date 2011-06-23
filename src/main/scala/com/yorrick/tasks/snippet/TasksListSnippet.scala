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

   private def viewTask(content : NodeSeq) : NodeSeq = {
    val result = Task.getTasks(taskImportance.is).flatMap(task => {
        val redirectPath = "/tasks/edition/"

        val imageTag = task.image match {
          case Some(image) =>
            val imageSource = "/tasks/image/" + task.id
            <img id="image" alt="Image de test"/> % Attribute(None, "src", Text(imageSource), Null)
          case None =>
            <span>Pas d'image pour cette tâche</span>
        }

        (
          "#label *"        #> (task.label + "(" + task.id + ", " + task.importance.toString + ")") &
          ".description *+" #> task.detail &
          "#image"          #> imageTag &
          "#editLink"       #> SHtml.link(redirectPath, () => currentTask(Full(task)), content \\ "a")
        ).apply(content)
      }
    )

    result
  }

}