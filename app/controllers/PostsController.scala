package controllers

import events._
import eventstore._
import eventstore.Transaction._
import models._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import support.Forms._

object PostsController extends PostsController(Global.MemoryImageActions.view(_.posts))
class PostsController(actions: ApplicationActions[Posts, PostEvent]) {
  import actions._

  /**
   * Blog content form definition.
   */
  val postContentForm = Form(mapping(
    "title"  -> trimmedText.verifying(minLength(3)),
    "body"   -> trimmedText.verifying(minLength(3)))(PostContent.apply)(PostContent.unapply))

  /**
   * Show an overview of the most recent blog posts.
   */
  def index = QueryAction { posts => implicit request =>
    Ok(views.html.posts.index(posts.mostRecent(20)))
  }

  /**
   * Show a specific blog post.
   */
  def show(id: PostId) = QueryAction { posts => implicit request =>
    posts.get(id) map { post =>
      Ok(views.html.posts.show(post, comments.commentContentForm))
    } getOrElse {
      notFound
    }
  }

  /**
   * Show and submit actions for adding a new blog post.
   */
  object add {
    def show = AuthenticatedQueryAction { user => _ => implicit request =>
      if (user.canAddPost) {
        Ok(views.html.posts.add(PostId.generate(), postContentForm))
      } else {
        notFound
      }
    }

    def submit(id: PostId) = AuthenticatedCommandAction { user => _ => implicit request =>
      if (user.canAddPost) {
        postContentForm.bindFromRequest.fold(
          formWithErrors =>
            abort(BadRequest(views.html.posts.add(id, formWithErrors))),
          postContent =>
            Changes(StreamRevision.Initial, PostAdded(id, user.id, postContent): PostEvent).commit(
              onCommit = Redirect(routes.PostsController.show(id)).flashing("info" -> "Post added."),
              onConflict = conflict => Conflict(views.html.posts.edit(id, conflict.actual, postContentForm.fill(postContent), conflict.events))))
      } else {
        abort(notFound)
      }
    }
  }

  /**
   * Show and submit actions for editing an existing blog post.
   */
  object edit {
    def show(id: PostId) = AuthenticatedQueryAction { user => posts => implicit request =>
      posts.get(id).filter(user.canEditPost) map { post =>
        Ok(views.html.posts.edit(post.id, post.revision, postContentForm.fill(post.content)))
      } getOrElse {
        notFound
      }
    }

    def submit(id: PostId, expected: StreamRevision) = AuthenticatedCommandAction { user => posts => implicit request =>
      posts.get(id).filter(user.canEditPost) map { post =>
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
  def delete(id: PostId, expected: StreamRevision) = AuthenticatedCommandAction { user => posts => implicit request =>
    posts.get(id).filter(user.canDeletePost) map { post =>
      Changes(expected, PostDeleted(id): PostEvent).commit(
        onCommit = Redirect(routes.PostsController.index).flashing("info" -> "Post deleted."),
        onConflict = conflict => Conflict(views.html.posts.index(posts.mostRecent(20), conflict.events)))
    } getOrElse {
      abort(notFound)
    }
  }

  /**
   * Add and delete comments.
   */
  object comments {
    /**
     * The form depends on the current user. If the current user is registered
     * the name is fixed and the comment will be associated to the user. Guest
     * users can fill in any name and will be treated as pseudonymous users.
     */
    def commentContentForm(implicit context: CurrentUserContext): Form[CommentContent] = Form(
      context.currentUser.registered.map { user =>
        single("body" -> trimmedText.verifying(minLength(3))).transform[CommentContent](
          body => CommentContent(Left(user.id), body),
          commentContent => commentContent.body)
      }.getOrElse {
        mapping(
          "name" -> tokenizedText.verifying(minLength(3)).transform[Either[UserId, String]](Right.apply, _.right.getOrElse("")),
          "body" -> trimmedText.verifying(minLength(3)))(CommentContent.apply)(CommentContent.unapply)
      })

    def add(postId: PostId, expected: StreamRevision) = CommandAction { posts => implicit request =>
      posts.get(postId).filter(request.currentUser.canAddComment) map { post =>
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

    def delete(postId: PostId, expected: StreamRevision, commentId: CommentId) = AuthenticatedCommandAction { user => posts => implicit request =>
      (for {
        post <- posts.get(postId)
        comment <- post.comments.get(commentId)
        if user.canDeleteComment(post, comment)
      } yield {
        Changes(expected, CommentDeleted(postId, commentId): PostEvent).commit(
          onCommit = Redirect(routes.PostsController.show(postId)).flashing("info" -> "Comment deleted."),
          onConflict = conflict => Conflict(views.html.posts.show(post, commentContentForm, conflict.events)))
      }).getOrElse {
        abort(notFound)
      }
    }
  }
}
