package com.yorrick.tasks.view

import net.liftweb.http.LiftView
import xml.NodeSeq
import com.yorrick.tasks.model.Task
import net.liftweb.common.{Failure, Empty, Full}
import com.yorrick.tasks.snippet.requestvars.{currentTask, taskImportance}

object TasksView extends LiftView {

  def dispatch = {
    case "edit" => edit _
    case "list" => list _
  }

  /**
   * Liste des taches, avec liens pour edition
   */
  private def list : NodeSeq =
    <lift:surround with="default" at="content">
      <h2>Liste des taches à partir de {taskImportance.is}</h2>
      <ul >
        <div class="lift:TasksList.viewTask">
          <li>
            <h3 id="label">Label de la tâche</h3>
            <p class="description">Description : </p>
            <img id="image"/>
            <p>
              <a id="editLink">Editer la tâche</a>
            </p>

          </li>
        </div>
      </ul>
     </lift:surround>



  private def edit : NodeSeq = {
    <lift:surround with="default" at="content">
      <lift:TasksEdition.editTask form="POST" multipart="true">
        <!--<h2>Edition de la tache</h2>
        <h3 id="label">Label : </h3>
        <h3 id="description">Description : </h3>
        <h3>Importance : <br/><span id="importance">Groupe de boutons</span></h3>
        <h3 id="image">Ajouter une image : </h3><br/>
        <h3 id="saveButton"/>
        <h3 id="previousButton"/>
        <h3 id="nextButton"/>-->
      </lift:TasksEdition.editTask>
    </lift:surround>
  }

//  private def editTask(task : Task) =
//    <lift:Tasks.editTask form="POST" multipart="true">
//      <h2>Edition de la tache {task.id}</h2>
//      <h3 id="label">Label : </h3>
//      <h3 id="description">Description : </h3>
//      <h3>Importance : <br/><span id="importance">Groupe de boutons</span></h3>
//      <h3 id="image">Ajouter une image : </h3><br/>
//      <h3 id="submitButton"/>
//    </lift:Tasks.editTask>



}
