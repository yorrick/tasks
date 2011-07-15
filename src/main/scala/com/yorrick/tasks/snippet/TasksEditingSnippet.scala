

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

//  case class TaskHolder (
//    var id : Option[Long] = None,
//    var label : String   = "",
//    var desc : String    = "",
//    var importance : TaskImportance.Value = TaskImportance.Normal,
//    var image : Option[(String, Array[Byte])] = Empty
//  )
  
//var taskToSave = new TaskHolder
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
//        case Some((null, _)) =>
          S.error("Emty upload")
          false
        case Some(image) if (!(image.mimeType.startsWith("image/"))) =>
//        case Some((mime, data)) if (!(mime.startsWith("image/"))) =>
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

    taskToSave.save
    imageToSave match {
      case Some(image) => image.save
      case None => // rien a faire
    }
    
//    // sauvegarde de la tache
//    Task2.create.label(taskToSave.label).detail(taskToSave.desc).importance(taskToSave.importance).save
//    // sauvegarde de l'image
//    taskToSave.image match {
//      case Some((mime, data)) => Image2.create.data(data).mimeType(mime).save
//      case None => // rien a faire
//    }
    
//    val taskId = taskToSave.id match {
//    case None => -1
//    case Some(id) => id
//    }
//    val image = taskToSave.image match {
//	    case Some((mime, data)) => Some(new Image2(data, mime))
//	    case None => None
//    }
//    Task.saveTask(new Task(taskId, taskToSave.label, taskToSave.desc, taskToSave.importance, image))

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

//    // lorsque'on provient de la page de liste, la tache à sauvegarder est la tache courante (requestParam)
//    currentTask.get match {
//      case Full(taskFromList) =>
//        // premier affichage du first stage, on va chercher les données de la current task
//        taskToSave.id         = Full(taskFromList.id.is)
//        taskToSave.label      = taskFromList.label.is
//        taskToSave.desc       = taskFromList.detail.is
//        taskToSave.importance = taskFromList.importance.is
//
//        taskFromList.image match {
//          case Some(image) => taskToSave.image = Full(image.mimeType.is, image.data.is)
//          case _ => // nothing to do
//        }
////        taskFromList.image match {
////        case Some(Image2(data, mime)) => taskToSave.image = Full(mime, data)
////        case _ => // nothing to do
////        }
//
//      case _ =>
//        // rien à faire
//    }

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
//          taskToSave.image = Some((mime, data))
        case _ => // nothing to do
      }
    }

//    val imageTag = taskToSave match {
//      case TaskHolder(Some(id), _, _, _, Some((_, _))) =>
//        val imageSource = "/tasks/image/" + id
//        <img id="image" alt="Image de la task"/> % Attribute(None, "src", Text(imageSource), Null)
//      case _ =>
//        <span>Pas d'image pour cette tâche</span>
//    }

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
