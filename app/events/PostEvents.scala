package events

import java.util.UUID

/**
 * Content that is always present for a blog post.
 */
case class PostContent(author: String, title: String, content: String)

/**
 * Domain events defining the life-cycle of a blog post. Since domain events will
 * be serialized to durable storage and may be retrieved for many years, they need
 * to be stable, with no or few external dependencies.
 */
sealed trait PostEvent
case class PostCreated(postId: UUID, content: PostContent) extends PostEvent
case class PostUpdated(postId: UUID, content: PostContent) extends PostEvent
case class PostDeleted(postId: UUID) extends PostEvent
