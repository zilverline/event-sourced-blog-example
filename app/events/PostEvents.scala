package events

import java.util.UUID
import play.api.libs.json._
import scala.util.control.Exception.catching
import support.JsonMapping._

/**
 * Strongly typed identifier for posts.
 */
case class PostId(uuid: UUID)
object PostId {
  def generate(): PostId = PostId(UUID.randomUUID())

  def fromString(s: String): Option[PostId] = s match {
    case PostIdRegex(uuid) => catching(classOf[RuntimeException]) opt { PostId(UUID.fromString(uuid)) }
    case _                 => None
  }

  implicit val PostIdFormat: Format[PostId] = valueFormat(apply)(unapply)

  private val PostIdRegex = """PostId\(([a-fA-F0-9-]{36})\)""".r
}

/**
 * Content that is always present for a blog post.
 */
case class PostContent(author: String, title: String, body: String)

/**
 * Domain events defining the life-cycle of a blog post. Since domain events will
 * be serialized to durable storage and may be retrieved for many years, they need
 * to be stable, with no or few external dependencies.
 */
sealed trait PostEvent {
  def postId: PostId
}
case class PostAdded(postId: PostId, content: PostContent) extends PostEvent
case class PostEdited(postId: PostId, content: PostContent) extends PostEvent
case class PostDeleted(postId: PostId) extends PostEvent

object PostEvent {
  implicit val PostContentFormat: Format[PostContent] = objectFormat("author", "title", "body")(PostContent.apply)(PostContent.unapply)

  implicit val PostEventFormat: Format[PostEvent] = typeChoiceFormat(
    "PostAdded" -> objectFormat("postId", "content")(PostAdded.apply)(PostAdded.unapply),
    "PostEdited" -> objectFormat("postId", "content")(PostEdited.apply)(PostEdited.unapply),
    "PostDeleted" -> objectFormat("postId")(PostDeleted.apply)(PostDeleted.unapply))
}
