package models

import events._

/**
 * A specific blog post with its current content.
 */
case class Post(id: PostId, content: PostContent)

/**
 * The current state of blog posts, derived from all committed PostEvents.
 */
case class Posts(byId: Map[PostId, Post] = Map.empty, all: Seq[PostId] = Vector.empty) extends (PostId => Post) {
  def apply(id: PostId) = byId(id)
  def get(id: PostId) = byId.get(id)
  def mostRecent(n: Int) = all.takeRight(n).reverse.map(byId)

  def apply(event: PostEvent) = event match {
    case PostAdded(id, content)  => copy(byId = byId.updated(id, Post(id, content)), all = all :+ id)
    case PostEdited(id, content) => copy(byId = byId.updated(id, byId(id).copy(content = content)))
    case PostDeleted(id)         => copy(byId = byId - id, all = all.filterNot(_ == id))
  }
}
object Posts {
  def fromHistory(events: PostEvent*): Posts = events.foldLeft(Posts())(_ apply _)
}
