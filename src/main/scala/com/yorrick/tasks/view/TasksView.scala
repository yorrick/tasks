package com.yorrick.tasks.view

import net.liftweb.http.LiftView
import xml.NodeSeq
import com.yorrick.tasks.model.Task
import net.liftweb.common.{Failure, Empty, Full}
import com.yorrick.tasks.snippet.requestvars.{currentTask, taskImportance}

object TasksView extends LiftView {

  def dispatch = {
    case "list" => list _
  }

  /**
   * Liste des taches, avec liens pour edition
   */
  private def list : NodeSeq =
    <lift:surround with="default" at="content">

      <h2>Liste des taches a partir de {taskImportance.is}</h2>
    	<p class="lift:Menu.item?name=taskCreation;a:class=taskCreationLink">Ajouter une tache</p>
      <ul >
        <div class="lift:TasksList.viewTask">
          <li>
            <h3 id="label">Label de la tache</h3>
            <p class="description">Description : </p>
            <img id="image"/>
            <p>
              <a id="editLink">Editer la tache</a>
              <a id="removeLink">Supprimer la tache</a>
            </p>
          </li>
        </div>
      </ul>
     </lift:surround>
  
}
