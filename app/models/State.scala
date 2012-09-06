package models

import events._
import eventstore._

case class State(posts: Posts = Posts(), users: Users = Users()) {
  def update(event: DomainEvent, revision: StreamRevision) = event match {
    case event: PostEvent => copy(posts = posts.update(event, revision))
    case event: UserEvent => copy(users = users.update(event, revision))
    case _                => sys.error("unknown event: " + event)
  }

  def updateMany(events: Seq[(DomainEvent, StreamRevision)]): State = events.foldLeft(this) { (state, event) =>
    state.update(event._1, event._2)
  }
}
