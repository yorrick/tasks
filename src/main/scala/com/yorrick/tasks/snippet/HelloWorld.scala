package com.yorrick.tasks {
package snippet {

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import com.yorrick.tasks.lib._
import Helpers._

class HelloWorld {
  lazy val date: Box[Date] = DependencyFactory.inject[Date] // inject the date

  // bind the date into the element with id "time"
  /*def howdy = {
    println("toto")
    "#time *" #> (date.map(_.toString) + "toto")
  }*/

  def howdy = Text("polo")
  
  /*
   lazy val date: Date = DependencyFactory.time.vend // create the date via factory

   def howdy = "#time *" #> date.toString
   */
}

}
}
