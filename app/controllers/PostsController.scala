package controllers

import events._
import eventstore._, Transaction._
import models._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import scala.annotation.tailrec
import support.Forms._

object PostsController extends PostsController(Global.persistence.memoryImage)
class PostsController(override val memoryImage: MemoryImage[State, PostEvent]) extends ApplicationController[PostEvent] {
  /**
   * Blog content form definition.
   */
  val postContentForm = Form(mapping(
    "title"  -> trimmedText.verifying(minLength(3)),
    "body"   -> trimmedText.verifying(minLength(3)))(PostContent.apply)(PostContent.unapply))

  /**
   * Show an overview of the most recent blog posts.
   */
  def index = QueryAction { state => implicit request =>
    Ok(views.html.posts.index(state.posts.mostRecent(20)))
  }

  /**
   * Show a specific blog post.
   */
  def show(id: PostId) = QueryAction { state => implicit request =>
    state.posts.get(id) map { post =>
      Ok(views.html.posts.show(post, comments.commentContentForm))
    } getOrElse {
      notFound
    }
  }

  /**
   * Show and submit actions for adding a new blog post.
   */
  object add {
    def show = ApplicationAction { implicit request =>
      Ok(views.html.posts.add(PostId.generate(), postContentForm))
    }

    def submit(id: PostId) = CommandAction { state => implicit request =>
      request.currentUser map { user =>
          postContentForm.bindFromRequest.fold(
            formWithErrors =>
              abort(BadRequest(views.html.posts.add(id, formWithErrors))),
            postContent =>
              Changes(StreamRevision.Initial, PostAdded(id, user.userId, postContent): PostEvent).commit(
                onCommit = Redirect(routes.PostsController.show(id)).flashing("info" -> "Post added."),
                onConflict = conflict => Conflict(views.html.posts.edit(id, conflict.actual, postContentForm.fill(postContent), conflict.events))))
      } getOrElse {
        abort(Unauthorized)
      }
    }
  }

  /**
   * Show and submit actions for editing an existing blog post.
   */
  object edit {
    def show(id: PostId) = QueryAction { state => implicit request =>
      state.posts.get(id).filter(_.isAuthoredByCurrentUser) map { post =>
        Ok(views.html.posts.edit(post.id, post.revision, postContentForm.fill(post.content)))
      } getOrElse {
        notFound
      }
    }

    def submit(id: PostId, expected: StreamRevision) = CommandAction { state => implicit request =>
      state.posts.get(id).filter(_.isAuthoredByCurrentUser) map { post =>
        postContentForm.bindFromRequest.fold(
          formWithErrors =>
            abort(BadRequest(views.html.posts.edit(id, expected, formWithErrors))),
          postContent =>
            Changes(expected, PostEdited(id, postContent): PostEvent).commit(
              onCommit = Redirect(routes.PostsController.show(id)).flashing("info" -> "Post saved."),
              onConflict = conflict => Conflict(views.html.posts.edit(id, conflict.actual, postContentForm.fill(postContent), conflict.events))))
      } getOrElse {
        abort(notFound)
      }
    }
  }

  /**
   * Delete a blog post.
   */
  def delete(id: PostId, expected: StreamRevision) = CommandAction { state => implicit request =>
    def deletedResult = Redirect(routes.PostsController.index).flashing("info" -> "Post deleted.")
    state.posts.get(id) map { post =>
      if (post.isAuthoredByCurrentUser)
        Changes(expected, PostDeleted(id): PostEvent).commit(
          onCommit = deletedResult,
          onConflict = conflict => Conflict(views.html.posts.index(state.posts.mostRecent(20), conflict.events)))
      else
        abort(notFound)
    } getOrElse {
      abort(deletedResult)
    }
  }

  /**
   * Add and delete comments.
   */
  object comments {
    val commentContentForm = Form(mapping(
      "commenter" -> trimmedText.verifying(minLength(3)),
      "body"      -> trimmedText.verifying(minLength(3)))(CommentContent.apply)(CommentContent.unapply))

    def add(postId: PostId, expected: StreamRevision) = CommandAction { state => implicit request =>
      state.posts.get(postId) map { post =>
        commentContentForm.bindFromRequest.fold(
          formWithErrors =>
            abort(BadRequest(views.html.posts.show(post, formWithErrors))),
          commentContent =>
            Changes(expected, CommentAdded(postId, post.nextCommentId, commentContent): PostEvent).commit(
              onCommit = Redirect(routes.PostsController.show(postId)).flashing("info" -> "Comment added."),
              onConflict = conflict => Conflict(views.html.posts.show(post, commentContentForm.fill(commentContent), conflict.events))))
      } getOrElse {
        abort(notFound)
      }
    }

    def delete(postId: PostId, expected: StreamRevision, commentId: CommentId) = CommandAction { state => implicit request =>
      state.posts.get(postId) map { post =>
        def deletedResult = Redirect(routes.PostsController.show(postId)).flashing("info" -> "Comment deleted.")
        post.comments.get(commentId) match {
          case None =>
            abort(deletedResult)
          case Some(comment) =>
            Changes(expected, CommentDeleted(postId, commentId): PostEvent).commit(
              onCommit = deletedResult,
              onConflict = conflict => Conflict(views.html.posts.show(post, commentContentForm, conflict.events)))
        }
      } getOrElse {
        abort(notFound)
      }
    }
  }

  /**
   * 404 Not Found response.
   */
  private[this] def notFound(implicit request: Request[_]): Result = NotFound(views.html.defaultpages.notFound(request, None))
}
