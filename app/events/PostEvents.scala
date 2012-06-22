package events

import java.util.UUID

case class PostContent(author: String, title: String, content: String)

sealed trait PostEvent
case class PostCreated(postId: UUID, content: PostContent) extends PostEvent
case class PostUpdated(postId: UUID, content: PostContent) extends PostEvent
