package controllers

import events._
import eventstore._
import models._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.Play.current
import support.Mappings._
import scala.concurrent.stm._
import views.html.defaultpages.{ notFound, todo }

object PostsController extends PostsController(MemoryImage[Posts, PostEvent](new fake.FakeEventStore)(Posts()) {
  (posts, commit) => posts.updateMany(commit.eventsWithRevision)
})
class PostsController(memoryImage: MemoryImage[Posts, PostEvent]) extends Controller {
  /**
   * The current blog posts from the memory image.
   */
  def posts(): Posts = memoryImage.get

  /**
   * Commits an event and applies it to the current state. If successful the
   * provided callback `f` is applied to the `commit`. Otherwise a conflict
   * result is returned.
   */
  private[this] def commit(expected: StreamRevision, event: PostEvent)(f: Commit[PostEvent] => Result): Result = {
    memoryImage.tryCommit(event.postId.toString, expected, event) match {
      case Left(conflict) => Conflict(todo())
      case Right(commit)  => f(commit)
    }
  }

  /**
   * Show an overview of the most recent blog posts.
   */
  def index = Action { implicit request =>
    Ok(views.html.posts.index(posts().mostRecent(20)))
  }

  /**
   * Show a specific blog post.
   */
  def show(id: PostId) = Action { implicit request =>
    posts().get(id) match {
      case Some(post) => Ok(views.html.posts.show(post))
      case None       => NotFound(notFound(request, None))
    }
  }

  /**
   * Show and submit actions for adding a new blog post.
   */
  object add {
    def show = Action { implicit request =>
      Ok(views.html.posts.add(PostId.generate(), postContentForm))
    }

    def submit(id: PostId) = Action { implicit request =>
      postContentForm.bindFromRequest.fold(
        formWithErrors => BadRequest(views.html.posts.add(id, formWithErrors)),
        postContent =>
          commit(StreamRevision.Initial, PostAdded(id, postContent)) { commit =>
            Redirect(routes.PostsController.show(id)).flashing("info" -> "Post added.")
          })
    }
  }

  /**
   * Show and submit actions for editing an existing blog post.
   */
  object edit {
    def show(id: PostId) = Action { implicit request =>
      posts().get(id) match {
        case Some(post) => Ok(views.html.posts.edit(post.id, post.revision, postContentForm.fill(post.content)))
        case None       => NotFound(notFound(request, None))
      }
    }

    def submit(id: PostId, revision: StreamRevision) = Action { implicit request =>
      postContentForm.bindFromRequest.fold(
        formWithErrors => BadRequest(views.html.posts.edit(id, revision, formWithErrors)),
        postContent =>
          commit(revision, PostEdited(id, postContent)) { commit =>
            Redirect(routes.PostsController.show(id)).flashing("info" -> "Post saved.")
          })
    }
  }

  /**
   * Delete a blog post.
   */
  def delete(id: PostId, revision: StreamRevision) = Action { implicit request =>
    commit(revision, PostDeleted(id)) { commit =>
      Redirect(routes.PostsController.index).flashing("info" -> "Post deleted.")
    }
  }

  /*
   * Blog content form definition.
   */
  val postContentForm = Form(mapping(
    "author" -> trimmedText.verifying(minLength(3)),
    "title" -> trimmedText.verifying(minLength(3)),
    "body" -> trimmedText.verifying(minLength(3)))(PostContent.apply)(PostContent.unapply))
}
