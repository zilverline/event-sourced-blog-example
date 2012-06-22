package controllers

import scala.concurrent.stm._

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._

import events._
import models._
import support.Mappings._

object PostsController extends Controller {

  val postContentMapping = mapping(
    "author" -> trimmedText.verifying(minLength(3)),
    "title" -> trimmedText.verifying(minLength(3)),
    "content" -> trimmedText.verifying(minLength(3)))(PostContent.apply)(PostContent.unapply)
  val postForm = Form(
    mapping(
      "id" -> uuid,
      "content" -> postContentMapping)(PostCreated.apply)(PostCreated.unapply))

  def index = Action {
    Ok(views.html.posts.index(Application.posts.current.single().byUuid.values.toSeq))
  }

  def renderCreate = Action {
    Ok(views.html.posts.create(postForm))
  }

  def submitCreate = Action { implicit request =>
    postForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.posts.create(formWithErrors)),
      postCreated => atomic { implicit txn =>
        Application.posts.apply(postCreated)
        Redirect(routes.PostsController.index)
      })
  }
}
