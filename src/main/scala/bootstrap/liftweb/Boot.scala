package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.java.sql.{Connection, DriverManager}
import _root_.com.yorrick.tasks.model._
import com.yorrick.tasks.view.TasksView
import com.yorrick.tasks.snippet._


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = 
	new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
			     Props.get("db.url") openOr 
			     "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
			     Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    // where to search snippet
    LiftRules.addToPackages("com.yorrick.tasks")
    Schemifier.schemify(true, Schemifier.infoF _, User)
    
    // url rewriting
    LiftRules.statelessRewrite.append({
      case RewriteRequest(ParsePath(List("account", accountName), _, _, _), _, _) =>
         RewriteResponse("viewAcct" :: Nil, Map("accountName" -> accountName))


      case RewriteRequest(ParsePath(list @ List("tasks", "edition", _*), _, _, _), _, _) =>
        RewriteResponse("tasks-management" :: "edit" :: Nil)

      case RewriteRequest(ParsePath(List("tasks", taskImportance), _, _, _), _, _) =>
         RewriteResponse("tasks-management" :: "list" :: Nil, Map("taskImportance" -> taskImportance))
    })

    // Custom dispatch for image generation
    LiftRules.dispatch.append {
      case Req(List("tasks", "image", taskId), _, _) =>
        () => Image.viewImage(taskId)
    }
    
    def test = User.currentUser match {
      case Full(user) => "Taches de " + user.firstName
      case _ => "Taches"
    }
    
    // build sitemap
    val createTaskMenu = Menu(Loc("taskCreation", Link("tasks-management" :: "edit" :: Nil, false, "/tasks/edition"), "Ajouter une tache"))
    val listTasksMenu  = Menu(Loc("tasksList",    Link("tasks-management" :: "list" :: Nil, true,  "/tasks/"), test), createTaskMenu)
    
    val entries = List(Menu("Home") / "index") :::
    			  // tasks
                  List(listTasksMenu) :::
                  // the User management menu items
                  User.sitemap :::
                  Nil

    LiftRules.uriNotFound.prepend(NamedPF("404handler"){
      case (req,failure) => NotFoundAsTemplate(
        ParsePath(List("exceptions","404"),"html",false,false))
    })
    
    LiftRules.setSiteMap(SiteMap(entries:_*))
    
    // view dispatching
    LiftRules.viewDispatch.append {
      case "tasks-management" :: "edit" :: Nil => Left(() => TemplateFinder.findAnyTemplate("templates-hidden/tasks/tasks" :: Nil) match {
        case Full(content) => Full(content)
        case _ => Empty
      })
      
      case "tasks-management" :: Nil => Right(TasksView)
    }

    // snippet dispatching
    LiftRules.snippetDispatch.append {
      case "TasksList"             => TasksListSnippet

      case "TasksEdition"          => S.snippetForClass("TasksEditionSnippet") openOr {
        println("creating new snippet for tasks")
        val instance = new TasksEditionSnippet
        instance.addName("TasksEditionSnippet")
        S.overrideSnippetForClass("TasksEditionSnippet", instance)
        instance
      }

    }

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // set character encoding
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    S.addAround(DB.buildLoanWrapper)
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}
