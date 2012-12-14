package models

import org.scalacheck._, Arbitrary.arbitrary, Prop.{ forAll, forAllNoShrink }
import events._, PostEventsSpec._
import eventstore._
import eventstore.fake.FakeEventStore
import scala.collection.immutable.SortedMap

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class UsersSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  "Users" should {
    val A = UserId.generate()
    val B = UserId.generate()
    val emailAddress = EmailAddress("john@example.com")
    val password = Password.fromPlainText("password")
    val password2 = Password.fromPlainText("password2")
    val displayName = "John Doe"
    val authenticationToken = AuthenticationToken.generate()
    val authenticationToken2 = AuthenticationToken.generate()

    "contain registered users" in {
      val users = given(UserRegistered(A, emailAddress, displayName, password))

      users.get(A) must beSome(RegisteredUser(A, StreamRevision(1), emailAddress, displayName, password))
    }

    "store most recent password for user" in {
      val users = given(UserRegistered(A, emailAddress, displayName, password), UserPasswordChanged(A, password2))

      users.get(A) must beSome(RegisteredUser(A, StreamRevision(2), emailAddress, displayName, password2))
    }

    "track current authentication token when logged in" in {
      val users = given(UserRegistered(A, emailAddress, displayName, password), UserLoggedIn(A, authenticationToken))

      users.withAuthenticationToken(authenticationToken) must beSome(RegisteredUser(A, StreamRevision(2), emailAddress, displayName, password, Some(authenticationToken)))
    }

    "remove current authentication token logged out" in {
      val users = given(UserRegistered(A, emailAddress, displayName, password), UserLoggedIn(A, authenticationToken), UserLoggedOut(A))

      users.withAuthenticationToken(authenticationToken) must beNone
      users.get(A) must beSome(RegisteredUser(A, StreamRevision(3), emailAddress, displayName, password, None))
    }

    "remove previous authentication token when when logged in again" in {
      val users = given(
        UserRegistered(A, emailAddress, displayName, password),
        UserLoggedIn(A, authenticationToken),
        UserLoggedIn(A, authenticationToken2))

      users.withAuthenticationToken(authenticationToken) must beNone
      users.withAuthenticationToken(authenticationToken2) must beSome(RegisteredUser(A, StreamRevision(3), emailAddress, displayName, password, Some(authenticationToken2)))
    }

    "load from history" in forAll(UserEventsSpec.eventsForMultipleUsers.arbitrary) { events =>
      val users = given(events: _*)
      users must_== users
    }
  }

  def given(events: UserEvent*) = {
    val eventStore = FakeEventStore.fromHistory(events)
    try {
      val commits = eventStore.reader.readCommits(StoreRevision.Initial, StoreRevision.Maximum)
      commits.flatMap(_.eventsWithRevision).foldLeft(Users())((users, event) => users.update(event._1, event._2))
    } finally {
      eventStore.close
    }
  }
}
