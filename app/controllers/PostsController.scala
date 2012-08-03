package controllers

import events._
import eventstore._
import models._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import scala.annotation.tailrec
import support.Mappings._

object PostsController extends PostsController(Global.persistence.memoryImage)
class PostsController(memoryImage: MemoryImage[Posts, PostEvent]) extends Controller {
  /**
   * The current blog posts from the memory image.
   */
  def posts(): Posts = memoryImage.get

  /**
   * Commits an event and applies it to the current state. If successful the
   * provided `onCommit` callback is invoked and its result returned. Otherwise
   * the `onConflict` is callback is invoked and its result returned.
   */
  private[this] def commit(expected: StreamRevision, event: PostEvent)(onCommit: => Result, onConflict: (StreamRevision, Seq[PostEvent]) => Result): Result = {
    def resolveConflict(committed: Seq[PostEvent], attempted: PostEvent): Either[Seq[PostEvent], PostEvent] = {
      val conflicting = committed.filter(PostEvent.conflictsWith(_, attempted))
      if (conflicting.isEmpty) Right(attempted)
      else Left(conflicting)
    }

    @tailrec def run(expected: StreamRevision, event: PostEvent): Result = memoryImage.tryCommit(event.postId.toString, expected, event) match {
      case Right(commit) => onCommit
      case Left(conflict) => resolveConflict(conflict.conflicting.flatMap(_.events), event) match {
        case Right(event)      => run(conflict.actual, event)
        case Left(conflicting) => onConflict(conflict.actual, conflicting)
      }
    }

    run(expected, event)
  }

  private[this] def notFound(request: Request[_]) = NotFound(views.html.defaultpages.notFound(request, None))

  private[this] def withPost(postId: PostId, notFound: Request[_] => Result = notFound)(found: Post => Result)(implicit request: Request[_]) = {
    posts().get(postId).map(found).getOrElse(notFound(request))
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
    withPost(id) { post =>
      Ok(views.html.posts.show(post, comments.commentContentForm))
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
          commit(StreamRevision.Initial, PostAdded(id, postContent))(
            onCommit = Redirect(routes.PostsController.show(id)).flashing("info" -> "Post added."),
            onConflict = (actual, conflicts) => Conflict(views.html.posts.edit(id, actual, postContentForm.fill(postContent), conflicts))))
    }
  }

  /**
   * Show and submit actions for editing an existing blog post.
   */
  object edit {
    def show(id: PostId) = Action { implicit request =>
      withPost(id) { post =>
        Ok(views.html.posts.edit(post.id, post.revision, postContentForm.fill(post.content)))
      }
    }

    def submit(id: PostId, expected: StreamRevision) = Action { implicit request =>
      withPost(id) { post =>
        postContentForm.bindFromRequest.fold(
          formWithErrors => BadRequest(views.html.posts.edit(id, expected, formWithErrors)),
          postContent =>
            commit(expected, PostEdited(id, postContent))(
              onCommit = Redirect(routes.PostsController.show(id)).flashing("info" -> "Post saved."),
              onConflict = (actual, conflicts) => Conflict(views.html.posts.edit(id, actual, postContentForm.fill(postContent), conflicts))))
      }
    }
  }

  /**
   * Delete a blog post.
   */
  def delete(id: PostId, expected: StreamRevision) = Action { implicit request =>
    def deletedResult = Redirect(routes.PostsController.index).flashing("info" -> "Post deleted.")
    withPost(id, notFound = _ => deletedResult) { post =>
      commit(expected, PostDeleted(id))(
        onCommit = deletedResult,
        onConflict = (actual, conflicts) => Conflict(views.html.posts.index(posts().mostRecent(20), conflicts)))
    }
  }

  /**
   * Add and delete comments.
   */
  object comments {
    val commentContentForm = Form(mapping(
      "commenter" -> trimmedText.verifying(minLength(3)),
      "body" -> trimmedText.verifying(minLength(3)))(CommentContent.apply)(CommentContent.unapply))

    def add(postId: PostId, expected: StreamRevision) = Action { implicit request =>
      withPost(postId) { post =>
        commentContentForm.bindFromRequest.fold(
          formWithErrors => BadRequest(views.html.posts.show(post, formWithErrors)),
          commentContent =>
            commit(expected, CommentAdded(postId, post.nextCommentId, commentContent))(
              onCommit = Redirect(routes.PostsController.show(postId)).flashing("info" -> "Comment added."),
              onConflict = (actual, conflicts) => Conflict(views.html.posts.show(post, commentContentForm.fill(commentContent), conflicts))))
      }
    }

    def delete(postId: PostId, expected: StreamRevision, commentId: CommentId) = Action { implicit request =>
      withPost(postId) { post =>
        def deletedResult = Redirect(routes.PostsController.show(postId)).flashing("info" -> "Comment deleted.")
        post.comments.get(commentId) match {
          case None => deletedResult
          case Some(comment) =>
            commit(expected, CommentDeleted(postId, commentId))(
              onCommit = deletedResult,
              onConflict = (actual, conflicts) => Conflict(views.html.posts.show(post, commentContentForm, conflicts)))
        }
      }
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
