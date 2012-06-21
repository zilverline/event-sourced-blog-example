package models

import java.util.UUID
import support.MemoryImage

sealed trait PostEvent
case class PostCreated(postId: UUID, author: String, title: String, content: String) extends PostEvent

case class Post(id: UUID, author: String, title: String, content: String)

case class Posts(byUuid: Map[UUID, Post] = Map.empty) {
  def apply(event: PostEvent) = event match {
    case PostCreated(id, author, title, content) => copy(byUuid = byUuid.updated(id, Post(id, author, title, content)))
  }
}
