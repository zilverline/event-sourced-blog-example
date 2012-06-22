package controllers

import play.api._
import play.api.mvc._

import events._
import models._
import support.MemoryImage

object Application extends Controller {

  val posts = MemoryImage[Posts, PostEvent](Posts()) { _ apply _ }

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

}
