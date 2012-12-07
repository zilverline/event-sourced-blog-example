package events

import eventstore.ConflictsWith
import eventstore.EventStreamType
import eventstore.JsonMapping._
import java.util.UUID
import play.api.libs.json._

/**
 * Strongly typed identifiers for posts and comments.
 */
case class PostId(uuid: UUID) extends Identifier
object PostId extends IdentifierCompanion[PostId]("PostId")

case class CommentId(value: Int)
object CommentId {
  implicit val CommentIdFormat: Format[CommentId] = valueFormat(apply)(unapply)
  implicit val CommentIdOrdering: Ordering[CommentId] = Ordering.by(_.value)
}

/**
 * Content that is always present for a blog post.
 */
case class PostContent(title: String, body: String)
case class CommentContent(commenter: String, body: String)

/**
 * Domain events defining the life-cycle of a blog post. Since domain events will
 * be serialized to durable storage and may be retrieved for many years, they need
 * to be stable, with no or few external dependencies.
 */
sealed trait PostEvent extends DomainEvent {
  def postId: PostId
}
case class PostAdded(postId: PostId, authorId: UserId, content: PostContent) extends PostEvent
case class PostEdited(postId: PostId, content: PostContent) extends PostEvent
case class PostDeleted(postId: PostId) extends PostEvent
sealed trait PostCommentEvent extends PostEvent {
  def commentId: CommentId
}
case class CommentAdded(postId: PostId, commentId: CommentId, content: CommentContent) extends PostCommentEvent
case class CommentDeleted(postId: PostId, commentId: CommentId) extends PostCommentEvent

object PostEvent {
  implicit val PostEventConflictsWith: ConflictsWith[PostEvent] = ConflictsWith {
    case (a: PostCommentEvent, b: PostCommentEvent) => a.commentId == b.commentId
    case (_: PostCommentEvent, _)                   => false
    case _                                          => true
  }

  implicit val PostEventStreamType: EventStreamType[PostId, PostEvent] = EventStreamType(_.toString, _.postId)

  implicit val PostContentFormat: Format[PostContent] = objectFormat("title", "body")(PostContent.apply)(PostContent.unapply)
  implicit val CommentContentFormat: Format[CommentContent] = objectFormat("commenter", "body")(CommentContent.apply)(CommentContent.unapply)

  implicit val PostEventFormat: TypeChoiceFormat[PostEvent] = TypeChoiceFormat(
    "PostAdded"      -> objectFormat("postId", "authorId", "content")(PostAdded.apply)(PostAdded.unapply),
    "PostEdited"     -> objectFormat("postId", "content")(PostEdited.apply)(PostEdited.unapply),
    "PostDeleted"    -> objectFormat("postId")(PostDeleted.apply)(PostDeleted.unapply),
    "CommentAdded"   -> objectFormat("postId", "commentId", "content")(CommentAdded.apply)(CommentAdded.unapply),
    "CommentDeleted" -> objectFormat("postId", "commentId")(CommentDeleted.apply)(CommentDeleted.unapply))
}
