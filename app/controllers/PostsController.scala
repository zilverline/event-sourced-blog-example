package controllers

import java.util.UUID

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

  val postContentForm = Form(mapping(
    "author" -> trimmedText.verifying(minLength(3)),
    "title" -> trimmedText.verifying(minLength(3)),
    "content" -> trimmedText.verifying(minLength(3)))(PostContent.apply)(PostContent.unapply))

  def index = Action {
    Ok(views.html.posts.index(Application.posts.current.single().last(20)))
  }

  def renderCreate = Action {
    Ok(views.html.posts.create(UUID.randomUUID, postContentForm))
  }

  def submitCreate(id: UUID) = Action { implicit request =>
    postContentForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.posts.create(id, formWithErrors)),
      content => atomic { implicit txn =>
        Application.posts.apply(PostCreated(id, content))
        Redirect(routes.PostsController.index)
      })
  }

  def renderEdit(id: UUID) = Action {
    Ok(views.html.posts.edit(id, postContentForm.fill(Application.posts.current.single().byId(id).content)))
  }

  def submitEdit(id: UUID) = Action { implicit request =>
    postContentForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.posts.edit(id, formWithErrors)),
      content => atomic { implicit txn =>
        Application.posts.apply(PostUpdated(id, content))
        Redirect(routes.PostsController.index)
      })
  }

  def delete(id: UUID) = Action {
    atomic { implicit txn =>
      Application.posts.apply(PostDeleted(id))
      Redirect(routes.PostsController.index)
    }
  }
}
