package controllers

import java.util.UUID
import scala.concurrent.stm._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import views.html.defaultpages.{ notFound, todo }
import events._
import models._
import support.Mappings._

object PostsController extends Controller {
  def eventStore = Application.eventStore
  def posts = Application.posts.current.single()

  val postContentForm = Form(mapping(
    "author" -> trimmedText.verifying(minLength(3)),
    "title" -> trimmedText.verifying(minLength(3)),
    "content" -> trimmedText.verifying(minLength(3)))(PostContent.apply)(PostContent.unapply))

  def index = Action { implicit request =>
    Ok(views.html.posts.index(posts.last(20)))
  }

  def show(id: UUID) = Action { implicit request =>
    posts.get(id) match {
      case Some(post) => Ok(views.html.posts.show(post))
      case None       => NotFound(notFound(request, None))
    }
  }

  def renderCreate = Action { implicit request =>
    Ok(views.html.posts.create(UUID.randomUUID, postContentForm))
  }

  def submitCreate(id: UUID) = Action { implicit request =>
    postContentForm.bindFromRequest.fold(
      errors => BadRequest(views.html.posts.create(id, errors)),
      content => eventStore.commit(id, 0, PostCreated(id, content)) {
        case Left(conflict) => Conflict(todo())
        case Right(commit)  => Redirect(routes.PostsController.index).flashing("info" -> "Post created.")
      })
  }

  def renderEdit(id: UUID) = Action { implicit request =>
    posts.get(id) match {
      case Some(post) => Ok(views.html.posts.edit(id, post.version, postContentForm.fill(post.content)))
      case None       => NotFound(notFound(request, None))
    }
  }

  def submitEdit(id: UUID, version: Int) = Action { implicit request =>
    postContentForm.bindFromRequest.fold(
      errors => BadRequest(views.html.posts.edit(id, version, errors)),
      content => eventStore.commit(id, version, PostUpdated(id, content)) {
        case Left(conflict) => Conflict(todo())
        case Right(commit)  => Redirect(routes.PostsController.index).flashing("info" -> "Post saved.")
      })
  }

  def delete(id: UUID, version: Int) = Action { implicit request =>
    eventStore.commit(id, version, PostDeleted(id)) {
      case Left(conflict) => Conflict(todo())
      case Right(commit)  => Redirect(routes.PostsController.index).flashing("info" -> "Post deleted.")
    }
  }
}
