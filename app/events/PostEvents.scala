package events

import java.util.UUID
import scala.util.control.Exception.catching

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

  private val PostIdRegex = """PostId\(([a-fA-F0-9-]{36})\)""".r
}

/**
 * Content that is always present for a blog post.
 */
case class PostContent(author: String, title: String, content: String)

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
