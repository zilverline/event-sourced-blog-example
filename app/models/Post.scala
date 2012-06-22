package models

import java.util.UUID
import support.MemoryImage

import events._

case class Post(id: UUID, content: PostContent)

case class Posts(byId: Map[UUID, Post] = Map.empty) {
  def apply(event: PostEvent) = event match {
    case PostCreated(id, content) => copy(byId = byId.updated(id, Post(id, content)))
    case PostUpdated(id, content) => copy(byId = byId.updated(id, Post(id, content)))
    case PostDeleted(id)          => copy(byId = byId - id)
  }
}
