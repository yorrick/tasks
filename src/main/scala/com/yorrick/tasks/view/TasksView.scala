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
      <h2>Liste des taches à partir de {taskImportance.is}</h2>
      <div class="lift:TasksList.createTask">
        <h3><a>Ajouter une tâche</a></h3>
      </div>
      <ul >
        <div class="lift:TasksList.viewTask">
          <li>
            <h3 id="label">Label de la tâche</h3>
            <p class="description">Description : </p>
            <img id="image"/>
            <p>
              <a id="editLink">Editer la tâche</a>
              <a id="removeLink">Supprimer la tâche</a>
            </p>
          </li>
        </div>
      </ul>
     </lift:surround>

}
