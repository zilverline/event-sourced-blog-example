package models

import events._
import eventstore._

case class ApplicationState(posts: Posts = Posts(), users: Users = Users()) {
  def update(event: DomainEvent, revision: StreamRevision) = event match {
    case event: PostEvent => copy(posts = posts.update(event, revision))
    case event: UserEvent => copy(users = users.update(event, revision))
    case _                => sys.error("unknown event: " + event)
  }

  def updateMany(events: Seq[(DomainEvent, StreamRevision)]): ApplicationState = events.foldLeft(this) {
    case (state, (event, streamRevision)) => state.update(event, streamRevision)
  }
}
