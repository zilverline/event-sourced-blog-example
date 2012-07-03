package models

import events._

/**
 * A specific blog post with its current content.
 */
case class Post(id: PostId, content: PostContent)

/**
 * The current state of blog posts, derived from all committed PostEvents.
 */
case class Posts(byId: Map[PostId, Post] = Map.empty, orderedByTimeAdded: Seq[PostId] = Vector.empty) {
  def get(id: PostId): Option[Post] = byId.get(id)
  def mostRecent(n: Int): Seq[Post] = orderedByTimeAdded.takeRight(n).reverse.map(byId)

  def apply(event: PostEvent): Posts = event match {
    case PostAdded(id, content) =>
      this.copy(byId = byId.updated(id, Post(id, content)), orderedByTimeAdded = orderedByTimeAdded :+ id)
    case PostEdited(id, content) =>
      this.copy(byId = byId.updated(id, byId(id).copy(content = content)))
    case PostDeleted(id) =>
      this.copy(byId = byId - id, orderedByTimeAdded = orderedByTimeAdded.filterNot(_ == id))
  }
}
object Posts {
  def fromHistory(events: PostEvent*): Posts = events.foldLeft(Posts())(_ apply _)
}
