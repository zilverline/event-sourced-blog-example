package controllers

import play.api._
import play.api.mvc._

import models._
import support.MemoryImage

object Application extends Controller {

  val posts = MemoryImage(Posts()) { (state, event: PostEvent) =>
    state
  }

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

}
