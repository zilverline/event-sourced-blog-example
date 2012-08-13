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
   * Blog content form definition.
   */
  val postContentForm = Form(mapping(
    "author" -> trimmedText.verifying(minLength(3)),
    "title" -> trimmedText.verifying(minLength(3)),
    "body" -> trimmedText.verifying(minLength(3)))(PostContent.apply)(PostContent.unapply))

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
    posts().get(id) map { post =>
      Ok(views.html.posts.show(post, comments.commentContentForm))
    } getOrElse notFound
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
        formWithErrors =>
          BadRequest(views.html.posts.add(id, formWithErrors)),
        postContent =>
          memoryImage.modify { _ =>
            Transaction.commit(Changes(StreamRevision.Initial, PostAdded(id, postContent)))(
              onCommit = Redirect(routes.PostsController.show(id)).flashing("info" -> "Post added."),
              onConflict = (actual, conflicts) => Conflict(views.html.posts.edit(id, actual, postContentForm.fill(postContent), conflicts)))
          })
    }
  }

  /**
   * Show and submit actions for editing an existing blog post.
   */
  object edit {
    def show(id: PostId) = Action { implicit request =>
      posts.get(id) map { post =>
        Ok(views.html.posts.edit(post.id, post.revision, postContentForm.fill(post.content)))
      } getOrElse notFound
    }

    def submit(id: PostId, expected: StreamRevision) = Action { implicit request =>
      updatePost(id) { post =>
        postContentForm.bindFromRequest.fold(
          formWithErrors =>
            Transaction.abort(BadRequest(views.html.posts.edit(id, expected, formWithErrors))),
          postContent =>
            Transaction.commit(Changes(expected, PostEdited(id, postContent)))(
              onCommit = Redirect(routes.PostsController.show(id)).flashing("info" -> "Post saved."): Result,
              onConflict = (actual, conflicts) => Conflict(views.html.posts.edit(id, actual, postContentForm.fill(postContent), conflicts))))
      } getOrElse notFound
    }
  }

  /**
   * Delete a blog post.
   */
  def delete(id: PostId, expected: StreamRevision) = Action { implicit request =>
    def deletedResult = Redirect(routes.PostsController.index).flashing("info" -> "Post deleted.")
    updatePost(id) { post =>
      Transaction.commit(Changes(expected, PostDeleted(id)))(
        onCommit = deletedResult,
        onConflict = (actual, conflicts) => Conflict(views.html.posts.index(posts().mostRecent(20), conflicts)))
    } getOrElse deletedResult
  }

  /**
   * Add and delete comments.
   */
  object comments {
    val commentContentForm = Form(mapping(
      "commenter" -> trimmedText.verifying(minLength(3)),
      "body" -> trimmedText.verifying(minLength(3)))(CommentContent.apply)(CommentContent.unapply))

    def add(postId: PostId, expected: StreamRevision) = Action { implicit request =>
      updatePost(postId) { post =>
        commentContentForm.bindFromRequest.fold(
          formWithErrors =>
            Transaction.abort(BadRequest(views.html.posts.show(post, formWithErrors))),
          commentContent =>
            Transaction.commit(Changes(expected, CommentAdded(postId, post.nextCommentId, commentContent)))(
              onCommit = Redirect(routes.PostsController.show(postId)).flashing("info" -> "Comment added."),
              onConflict = (actual, conflicts) => Conflict(views.html.posts.show(post, commentContentForm.fill(commentContent), conflicts))))
      } getOrElse notFound
    }

    def delete(postId: PostId, expected: StreamRevision, commentId: CommentId) = Action { implicit request =>
      updatePost(postId) { post =>
        def deletedResult = Redirect(routes.PostsController.show(postId)).flashing("info" -> "Comment deleted.")
        post.comments.get(commentId) match {
          case None =>
            Transaction.abort(deletedResult)
          case Some(comment) =>
            Transaction.commit(Changes(expected, CommentDeleted(postId, commentId)))(
              onCommit = deletedResult,
              onConflict = (actual, conflicts) => Conflict(views.html.posts.show(post, commentContentForm, conflicts)))
        }
      } getOrElse notFound
    }
  }

  /**
   * The current blog posts from the memory image.
   */
  private[this] def posts(): Posts = memoryImage.get

  /**
   * 404 Not Found response.
   */
  private[this] def notFound(implicit request: Request[_]): Result = NotFound(views.html.defaultpages.notFound(request, None))

  /**
   * Runs the transaction `body` against the post identified by `postId` and
   * returns the result, if it exists. Otherwise `None` is returned.
   */
  private[this] def updatePost[A](id: PostId)(body: Post => Transaction[PostEvent, A]): Option[A] = {
    memoryImage.modify { posts =>
      posts.get(id).map { post =>
        body(post).map(a => Some(a))
      }.getOrElse {
        Transaction.abort(None)
      }
    }
  }
}
