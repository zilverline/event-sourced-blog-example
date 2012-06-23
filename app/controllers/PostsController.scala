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
import views.html.defaultpages._
import eventstore.Commit

object PostsController extends Controller {
  import Application.{ eventStore, posts }

  val postContentForm = Form(mapping(
    "author" -> trimmedText.verifying(minLength(3)),
    "title" -> trimmedText.verifying(minLength(3)),
    "content" -> trimmedText.verifying(minLength(3)))(PostContent.apply)(PostContent.unapply))

  def index = Action { implicit request =>
    Ok(views.html.posts.index(Application.posts.current.single().last(20)))
  }

  def show(id: UUID) = Action { implicit request =>
    posts.current.single().byId.get(id) match {
      case Some(post) => Ok(views.html.posts.show(post))
      case None       => NotFound(notFound(request, None))
    }
  }

  def renderCreate = Action { implicit request =>
    Ok(views.html.posts.create(UUID.randomUUID, postContentForm))
  }

  def submitCreate(id: UUID) = Action { implicit request =>
    postContentForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.posts.create(id, formWithErrors)),
      content => eventStore.commit(id, 0, PostCreated(id, content)) {
        case Left(conflict) => Conflict(todo())
        case Right(commit)  => Redirect(routes.PostsController.index).flashing("info" -> "Post created.")
      })
  }

  def renderEdit(id: UUID) = Action { implicit request =>
    posts.current.single().byId.get(id) match {
      case Some(post) => Ok(views.html.posts.edit(id, post.version, postContentForm.fill(post.content)))
      case None       => NotFound(notFound(request, None))
    }
  }

  def submitEdit(id: UUID) = Action { implicit request =>
    withVersion { version =>
      postContentForm.bindFromRequest.fold(
        formWithErrors => BadRequest(views.html.posts.edit(id, version, formWithErrors)),
        content => eventStore.commit(id, version, PostUpdated(id, content)) {
          case Left(conflict) => Conflict(todo())
          case Right(commit)  => Redirect(routes.PostsController.index).flashing("info" -> "Post saved.")
        })
    }
  }

  def delete(id: UUID) = Action { implicit request =>
    withVersion { version =>
      eventStore.commit(id, version, PostDeleted(id)) {
        case Left(conflict) => Conflict(todo())
        case Right(commit)  => Redirect(routes.PostsController.index).flashing("info" -> "Post deleted.")
      }
    }
  }

  private def withVersion(f: Int => Result)(implicit request: Request[_]): Result = {
    Form(single("version" -> number)).bindFromRequest.fold(
      _ => BadRequest(badRequest(request, "missing version")),
      f)
  }
}
