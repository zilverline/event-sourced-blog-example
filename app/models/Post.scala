package models

import java.util.UUID
import events._

/**
 * A specific blog post with its current content.
 */
case class Post(id: UUID, content: PostContent)

/**
 * The current state of blog posts, derived from all committed PostEvents.
 */
case class Posts(byId: Map[UUID, Post] = Map.empty, all: Seq[UUID] = Vector.empty) {
  def get(id: UUID) = byId.get(id)
  def mostRecent(n: Int) = all.takeRight(n).reverse.map(byId)

  def apply(event: PostEvent) = event match {
    case PostCreated(id, content) => copy(byId = byId.updated(id, Post(id, content)), all = all :+ id)
    case PostUpdated(id, content) => copy(byId = byId.updated(id, byId(id).copy(content = content)))
    case PostDeleted(id)          => copy(byId = byId - id, all = all.filterNot(_ == id))
  }
}
