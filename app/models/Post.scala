package models

import events._
import eventstore.StreamRevision
import scala.collection.immutable.SortedMap

/**
 * A specific blog post with its current revision and content.
 */
case class Post(
  id: PostId,
  revision: StreamRevision,
  author: UserId,
  content: PostContent,
  nextCommentId: CommentId = CommentId(1),
  comments: SortedMap[CommentId, CommentContent] = SortedMap.empty)

/**
 * The current state of blog posts, derived from all committed PostEvents.
 */
case class Posts(byId: Map[PostId, Post] = Map.empty, orderedByTimeAdded: Seq[PostId] = Vector.empty) {
  def get(id: PostId): Option[Post] = byId.get(id)
  def mostRecent(n: Int): Seq[Post] = orderedByTimeAdded.takeRight(n).reverse.map(byId)

  def update(event: PostEvent, revision: StreamRevision): Posts = event match {
    case PostAdded(id, author, content) =>
      this.copy(byId = byId.updated(id, Post(id, revision, author, content)), orderedByTimeAdded = orderedByTimeAdded :+ id)
    case PostDeleted(id) =>
      this.copy(byId = byId - id, orderedByTimeAdded = orderedByTimeAdded.par.filterNot(_ == id).seq)
    case PostEdited(id, content) =>
      modify(id) { post => post.copy(revision = revision, content = content) }
    case CommentAdded(id, commentId, content) =>
      modify(id) { post =>
        post.copy(
          revision = revision,
          nextCommentId = CommentId(commentId.value + 1),
          comments = post.comments.updated(commentId, content))
      }
    case CommentDeleted(id, commentId) =>
      modify(id) { post =>
        post.copy(
          revision = revision,
          comments = post.comments - commentId)
      }
  }

  private[this] def modify(id: PostId)(f: Post => Post) = this.copy(byId = byId.updated(id, f(byId(id))))
}
