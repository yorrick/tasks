

package com.yorrick.tasks {
package snippet {

import net.liftweb.util.BindHelpers._
import requestvars.currentTask
import net.liftweb.http._
import model.{Image2, Task2, TaskImportance}
import xml.{NodeSeq, Attribute, Text, Null}
import net.liftweb.common.{Empty, Full, Box}

class TasksEditionSnippet extends StatefulSnippet {

  var dispatch : DispatchIt = {
    case "editTask" => firstStage _
  }

  val taskToSave : Task2 = currentTask.get match {
    case Full(existingTask) => existingTask
    case _ => Task2.create.label("").detail("").importance(TaskImportance.Normal)
  }
  
  var imageToSave : Option[Image2] = currentTask.get match {
    case Full(existingTask) => existingTask.image
    case _ => None
  }


  /**
   * Implémente les controles
   */
  def controlTask() : Boolean = {
    val minLabelLenght = 5

    // controles
    if (taskToSave.label.is.size < minLabelLenght) {
      S.error("label must be at least " + minLabelLenght + " chars long")
      false
    } else {
      imageToSave match {
        case None => true
        // An empty upload gets reported with a null mime type,
        // so we need to handle this special case
        case Some(image) if (image.mimeType == null) =>
          S.error("Emty upload")
          false
        case Some(image) if (!(image.mimeType.startsWith("image/"))) =>
          S.error("Please upload an image")
          false
        case Some(_) => true
      }
    }
  }

  /**
   * Sauvegarde des données
   */
  def saveData = {
    println("trying to save task " + taskToSave)

    // sauvegarde de la tache et de l'image
    taskToSave.save
    imageToSave match {
      case Some(image) => image.save
      case None => // rien a faire
    }
    
    // on supprime le snippet de la session, et redirection
    unregisterThisSnippet()
    S.redirectTo("/tasks/")
  }

  /**
   * Rendu de la premiere etape (informations de la tache)
   * @param c
   * @return
   */
  private def firstStage(c : NodeSeq) : NodeSeq = {
    println("firstStage")

    val content = TemplateFinder.findAnyTemplate("templates-hidden/tasks/stage1" :: Nil) openOr <span>Could not load template</span>

    def saveFirstStageData = {
      if (controlTask){
        saveData
      }
    }

    def goToSecondStage = {
      if (controlTask){
        dispatch = {
          case "editTask" => secondStage _
        }
      }

    }

    // liste pour le choice
    val options = List(
      (TaskImportance.Important, "Important"),
      (TaskImportance.Normal,    "Normal"),
      (TaskImportance.Low,       "Faible"))

    val generatedXml = (
      "#label *+"       #> SHtml.text(taskToSave.label.is, taskToSave.label(_), "maxlength" -> "20", "cols" -> "20") &
      "#description *+" #> SHtml.textarea(taskToSave.detail.is, taskToSave.detail(_), "cols" -> "30", "rows" -> "8") &
      "#importance"     #> SHtml.selectObj(options, Full(taskToSave.importance.is), {imp : TaskImportance.Value => taskToSave.importance(imp)}) &
      "#saveButton"     #> SHtml.submit("Sauvegarder", saveFirstStageData _) &
      "#nextButton"     #> SHtml.submit("Ajouter une image", goToSecondStage _)
    ).apply(content)

    generatedXml
  }

  /**
   * Rendu pour la seconde etape (ajout de l'image)
   */
  private def secondStage(c : NodeSeq) : NodeSeq = {
    def handleFileUpload : FileParamHolder => Any = {holder : FileParamHolder =>
      holder match {
        case FileParamHolder(name, mime, fileName, data) =>
        	imageToSave match {
        	  case None => imageToSave = Some(Image2.create.mimeType(mime).data(data).task(taskToSave))
        	  case Some(image) => image.mimeType(mime).data(data)
        	}
        case _ => // nothing to do
      }
    }

    def saveFirstAndSecondStageData = {
      if (controlTask){
        saveData
      }
    }

    def previous = {
      dispatch = {
        case "editTask" => firstStage _
      }
    }

    val content = TemplateFinder.findAnyTemplate("templates-hidden/tasks/stage2" :: Nil) openOr <span>Could not load template</span>

    val generatedXml = (
      "#imageViewing"   #> imageTag &
      "#image"          #> SHtml.fileUpload(handleFileUpload) &
      "#previousButton" #> SHtml.submit("Précédent", previous _) &
      "#saveButton"     #> SHtml.submit("Sauvegarder", saveFirstAndSecondStageData _)
    ).apply(content)

    generatedXml
  }
  
  /**
   * Contruit le tag pour l'image
   */
  private def imageTag : NodeSeq = if (taskToSave.saved_?) {
      imageToSave match {
        case Some(image) =>
          val imageSource = "/tasks/image/" + taskToSave.id
          <img id="image" alt="Image de la task"/> % Attribute(None, "src", Text(imageSource), Null)
        case None => <span>Pas d'image pour cette tâche</span>
      }
    } else {
      <span>Tâche pas encore enregistrée, affichage de l'image impossible</span>
    }

}

}
}
