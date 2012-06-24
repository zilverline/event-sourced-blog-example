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

  val memoryImage = {
    val initialEvents = Seq(
      PostCreated(UUID.fromString("4e885ffe-870e-45b4-b5dd-f16d381d6f6a"), PostContent("Erik", "Scala is awesome", "Scala...")),
      PostCreated(UUID.fromString("4e885ffe-870e-45b4-b5dd-f16d381d6f6f"), PostContent("Bas", "Righteous Ruby", "Ruby...")))
    val initialValue = initialEvents.foldLeft(Posts())(_ apply _)
    Ref(initialValue).single
  }

  def posts = memoryImage()
  def commit(event: PostEvent) = memoryImage.transform(_.apply(event))

  val postContentForm = Form(mapping(
    "author" -> trimmedText.verifying(minLength(3)),
    "title" -> trimmedText.verifying(minLength(3)),
    "content" -> trimmedText.verifying(minLength(3)))(PostContent.apply)(PostContent.unapply))

  def index = Action { implicit request =>
    Ok(views.html.posts.index(posts.mostRecent(20)))
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
      formWithErrors => BadRequest(views.html.posts.create(id, formWithErrors)),
      postContent => {
        commit(PostCreated(id, postContent))
        Redirect(routes.PostsController.index).flashing("info" -> "Post created.")
      })
  }

  def renderEdit(id: UUID) = Action { implicit request =>
    posts.get(id) match {
      case Some(post) => Ok(views.html.posts.edit(id, postContentForm.fill(post.content)))
      case None       => NotFound(notFound(request, None))
    }
  }

  def submitEdit(id: UUID) = Action { implicit request =>
    postContentForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.posts.edit(id, formWithErrors)),
      postContent => {
        commit(PostUpdated(id, postContent))
        Redirect(routes.PostsController.index).flashing("info" -> "Post saved.")
      })
  }

  def delete(id: UUID) = Action { implicit request =>
    commit(PostDeleted(id))
    Redirect(routes.PostsController.index).flashing("info" -> "Post deleted.")
  }
}
