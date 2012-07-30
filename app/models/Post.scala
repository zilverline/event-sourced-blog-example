package models

import events._
import eventstore._

/**
 * A specific blog post with its current revision and content.
 */
case class Post(id: PostId, revision: StreamRevision, content: PostContent)

/**
 * The current state of blog posts, derived from all committed PostEvents.
 */
case class Posts(byId: Map[PostId, Post] = Map.empty, orderedByTimeAdded: Seq[PostId] = Vector.empty) {
  def get(id: PostId): Option[Post] = byId.get(id)
  def mostRecent(n: Int): Seq[Post] = orderedByTimeAdded.takeRight(n).reverse.map(byId)

  def update(event: PostEvent, revision: StreamRevision): Posts = event match {
    case PostAdded(id, content) =>
      this.copy(byId = byId.updated(id, Post(id, revision, content)), orderedByTimeAdded = orderedByTimeAdded :+ id)
    case PostEdited(id, content) =>
      this.copy(byId = byId.updated(id, byId(id).copy(revision = revision, content = content)))
    case PostDeleted(id) =>
      this.copy(byId = byId - id, orderedByTimeAdded = orderedByTimeAdded.par.filterNot(_ == id).seq)
  }

  def updateMany(events: Seq[(PostEvent, StreamRevision)]): Posts = events.foldLeft(this) { (posts, event) =>
    posts.update(event._1, event._2)
  }
}
object Posts {
  def fromHistory(events: Seq[(PostEvent, StreamRevision)]): Posts = Posts().updateMany(events)
}
