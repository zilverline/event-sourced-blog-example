package controllers

import play.api._
import play.api.mvc._
import eventstore.MemoryImage
import models.ApplicationState

object Application extends ApplicationController[Nothing] {
  override def memoryImage = Global.persistence.memoryImage

  def index = ApplicationAction { implicit request =>
    Ok(views.html.index())
  }
}
