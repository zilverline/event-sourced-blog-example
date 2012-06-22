package models

import java.util.UUID
import support.MemoryImage

import events._

case class Post(id: UUID, content: PostContent)

case class Posts(byUuid: Map[UUID, Post] = Map.empty) {
  def apply(event: PostEvent) = event match {
    case PostCreated(id, content) => copy(byUuid = byUuid.updated(id, Post(id, content)))
  }
}
