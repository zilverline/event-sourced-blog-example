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
    authorId: UserId,
    content: PostContent,
    nextCommentId: CommentId = CommentId(1),
    comments: SortedMap[CommentId, Comment] = SortedMap.empty) {

  def author(implicit context: UserContext): User = context.users.get(authorId) getOrElse UnknownUser(authorId)
  def isAuthoredBy(user: RegisteredUser) = authorId == user.id
}

case class Comment(id: CommentId, content: CommentContent) {
  def commenter(implicit context: UserContext): User =
    content.commenter.fold(
      userId => context.users.get(userId) getOrElse UnknownUser(userId),
      PseudonymousUser)

  def isAuthoredBy(user: RegisteredUser) = content.commenter.left.exists(_ == user.id)
}

/**
 * The current state of blog posts, derived from all committed PostEvents.
 */
case class Posts(
    private val byId: Map[PostId, Post] = Map.empty,
    private val orderedByTimeAdded: Seq[PostId] = Vector.empty) {

  def get(id: PostId): Option[Post] = byId.get(id)
  def mostRecent(n: Int): Seq[Post] = orderedByTimeAdded.takeRight(n).reverse.flatMap(get)

  def update(event: PostEvent, revision: StreamRevision): Posts = event match {
    case PostAdded(id, authorId, content) =>
      this.copy(byId = byId.updated(id, Post(id, revision, authorId, content)), orderedByTimeAdded = orderedByTimeAdded :+ id)

    case PostDeleted(id) =>
      this.copy(byId = byId - id, orderedByTimeAdded = orderedByTimeAdded.par.filterNot(_ == id).seq)

    case PostEdited(id, content) =>
      modify(id) { post => post.copy(revision = revision, content = content) }

    case CommentAdded(id, commentId, content) =>
      modify(id) { post =>
        val comment = Comment(commentId, content)
        post.copy(
          revision = revision,
          nextCommentId = CommentId(commentId.value + 1),
          comments = post.comments.updated(commentId, comment))
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
