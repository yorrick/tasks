package com.yorrick.tasks.snippet

import requestvars.{currentTask, taskImportance}
import net.liftweb.http.{SHtml, DispatchSnippet}
import xml.NodeSeq._
import xml.{Attribute, Null, NodeSeq, Text}
import net.liftweb.common.Full
import net.liftweb.util.BindHelpers._

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import com.yorrick.tasks.lib._
import Helpers._

object HomeSnippet extends DispatchSnippet{

  lazy val date: Box[Date] = DependencyFactory.inject[Date] // inject the date
  
  def dispatch : DispatchIt = {
    case "display" => display _
  }
  
  def display(content : NodeSeq) = {
    "#time *" #> (date openOr(new Date)).toString 
  }.apply(content)
  
}