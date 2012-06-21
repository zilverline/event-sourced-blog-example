package controllers

import play.api._
import play.api.mvc._

import models._

object PostsController extends Controller {

  def index = Action {
    Ok(views.html.posts.index(Application.posts.current.single().byUuid.values.toSeq))
  }

  def create = Action {
    Ok(views.html.posts.create())
  }

  def post = Action {
    Ok("")
  }
}
