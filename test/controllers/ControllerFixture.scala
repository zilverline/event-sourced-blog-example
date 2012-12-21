package controllers

import events._
import eventstore._
import models._
import play.api._
import play.api.test._
import org.specs2.mutable.After

object ControllerFixture {
  val password = Password.fromPlainText("password")
}
trait ControllerFixture extends After {
  Play.start(FakeApplication(additionalConfiguration = Map("eventstore.implementation" -> "fake")))

  val eventStore: EventStore[DomainEvent] = new fake.FakeEventStore

  var initialStoreRevision = eventStore.reader.storeRevision
  def given[StreamId, Event <: DomainEvent](events: Event*)(implicit eventStreamType: EventStreamType[StreamId, Event]) = {
    for (event <- events) {
      val revision = eventStore.reader.streamRevision(eventStreamType.streamId(event))
      eventStore.committer.tryCommit(Changes(revision, event))
    }
    initialStoreRevision = eventStore.reader.storeRevision
  }

  val memoryImage = MemoryImage[ApplicationState, DomainEvent](eventStore)(ApplicationState()) {
    (state, commit) => state.updateMany(commit.eventsWithRevision)
  }

  val currentUserId = UserId.generate()
  val authenticationToken = AuthenticationToken.generate()
  val unauthenticatedRequest = FakeRequest()
  val authenticatedRequest = FakeRequest().withSession("authenticationToken" -> authenticationToken.toString)

  given(
    UserRegistered(currentUserId, EmailAddress("joe@example.com"), "Joe", ControllerFixture.password): UserEvent,
    UserLoggedIn(currentUserId, authenticationToken))
  val registeredUser = memoryImage.get.users.withAuthenticationToken(authenticationToken) getOrElse (sys.error("user not authenticated"))

  def commits: Stream[eventstore.Commit[DomainEvent]] = eventStore.reader.readCommits(initialStoreRevision, StoreRevision.Maximum)
  def changes: Seq[DomainEvent] = commits.flatMap(_.events).toSeq

  override def after {
    eventStore.close
    Play.stop()
  }
}
