package models

import java.util.UUID
import support.MemoryImage

import events._

case class Post(id: UUID, version: Int, content: PostContent)

case class Posts(byId: Map[UUID, Post] = Map.empty, all: Seq[UUID] = Vector.empty) {
  def apply(event: PostEvent) = event match {
    case PostCreated(id, content) => copy(byId = byId.updated(id, Post(id, 1, content)), all = all :+ id)
    case PostUpdated(id, content) => copy(byId = byId.updated(id, byId(id).copy(version = byId(id).version + 1, content = content)))
    case PostDeleted(id)          => copy(byId = byId - id, all = all.filterNot(_ == id))
  }

  def get(id: UUID) = byId.get(id)
  def last(n: Int) = all.takeRight(n).reverse.map(byId)
}
