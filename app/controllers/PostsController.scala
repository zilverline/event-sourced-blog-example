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

  implicit def reader = (posts: Posts, id: PostId) => posts.get(id)

  implicit val PostEventConflictResolver = ConflictResolver(PostEvent.conflictsWith)

  private[this] def notFound(request: Request[_]) = NotFound(views.html.defaultpages.notFound(request, None))

  private[this] def withPost(postId: PostId, notFound: Request[_] => Result = notFound)(found: Post => Result)(implicit request: Request[_]) = {
    posts().get(postId).map(found).getOrElse(notFound(request))
  }

  private[this] def createPost(postId: PostId)(body: => Transaction[PostEvent, Result])(implicit request: Request[_]): Result = {
    memoryImage.modify(postId.toString, StreamRevision.Initial) { _ => body }
  }

  private[this] def updatePost(postId: PostId, expected: StreamRevision, notFound: Request[_] => Result = notFound)(body: Post => Transaction[PostEvent, Result])(implicit request: Request[_]): Result = {
    memoryImage.modify(postId.toString, expected) { posts =>
      posts.get(postId) match {
        case None       => Transaction.abort(notFound(request))
        case Some(post) => body(post)
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
        postContent => createPost(id) {
          Transaction.commit(PostAdded(id, postContent))(
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
      withPost(id) { post =>
        Ok(views.html.posts.edit(post.id, post.revision, postContentForm.fill(post.content)))
      }
    }

    def submit(id: PostId, expected: StreamRevision) = Action { implicit request =>
      updatePost(id, expected) { post =>
        postContentForm.bindFromRequest.fold(
          formWithErrors => Transaction.abort(BadRequest(views.html.posts.edit(id, expected, formWithErrors))),
          postContent =>
            Transaction.commit(PostEdited(id, postContent))(
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
    updatePost(id, expected, notFound = _ => deletedResult) { post =>
      Transaction.commit(PostDeleted(id))(
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
      updatePost(postId, expected) { post =>
        commentContentForm.bindFromRequest.fold(
          formWithErrors => Transaction.abort(BadRequest(views.html.posts.show(post, formWithErrors))),
          commentContent =>
            Transaction.commit(CommentAdded(postId, post.nextCommentId, commentContent))(
              onCommit = Redirect(routes.PostsController.show(postId)).flashing("info" -> "Comment added."),
              onConflict = (actual, conflicts) => Conflict(views.html.posts.show(post, commentContentForm.fill(commentContent), conflicts))))
      }
    }

    def delete(postId: PostId, expected: StreamRevision, commentId: CommentId) = Action { implicit request =>
      updatePost(postId, expected) { post =>
        def deletedResult = Redirect(routes.PostsController.show(postId)).flashing("info" -> "Comment deleted.")
        post.comments.get(commentId) match {
          case None => Transaction.abort(deletedResult)
          case Some(comment) =>
            Transaction.commit(CommentDeleted(postId, commentId))(
              onCommit = deletedResult,
              onConflict = (actual, conflicts) => Conflict(views.html.posts.show(post, commentContentForm, conflicts)))
        }
      }
    }
  }
}
