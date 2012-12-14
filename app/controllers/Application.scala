package controllers

import play.api._
import play.api.mvc._
import eventstore.MemoryImage
import models.ApplicationState

object Application {
  import Global.MemoryImageActions._

  def index = ApplicationAction { implicit request =>
    Ok(views.html.index())
  }
}
