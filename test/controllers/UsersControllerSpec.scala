package controllers

import play.api.test._
import play.api.test.Helpers._
import events._
import eventstore._
import models._
import org.specs2.mutable.After
import scala.collection.immutable.SortedMap
import eventstore.EventStreamType
import play.api.Play

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class UsersControllerSpec extends org.specs2.mutable.Specification {
  val userId = UserId.generate()
  val email = EmailAddress("john@example.com")
  val password = Password.fromPlainText("password")
  val authenticationToken = AuthenticationToken.generate

  "users controller" should {
    "register a new user" in new fixture {
      claimedUserIds += email -> userId
      val request = FakeRequest().withFormUrlEncodedBody("email" -> "john@example.com", "password.1" -> "password", "password.2" -> "password")

      val result = subject.register.submit(request)

      status(result) must_== 303
      val user = users.get(EmailAddress("john@example.com")) getOrElse { failure("user not registered") }
      user.emailAddress must_== EmailAddress("john@example.com")
      user.password.verify("password") aka "password verified" must beTrue
      user.userId aka "claimed user id" must_== userId
    }

    "allow registered user to log in" in new fixture {
      given(UserRegistered(userId, email, password))

      val result = subject.authentication.submit(FakeRequest().withFormUrlEncodedBody("email" -> email.value, "password" -> "password"))

      status(result) must_== 303
      val token = session(result).get("authenticationToken").flatMap(AuthenticationToken.fromString) getOrElse { failure("authentication token not created") }
      val user = users.authenticated(token) getOrElse { failure("token not mapped to user") }
      user.emailAddress must_== email
    }

    "allow logged in users to log out" in new fixture {
      given(UserRegistered(userId, email, password), UserLoggedIn(userId, authenticationToken))

      val result = subject.authentication.logOut(FakeRequest().withSession("authenticationToken" -> authenticationToken.toString))

      status(result) must_== 303
      session(result) must beEmpty
      users.authenticated(authenticationToken) must beNone
    }
  }

  trait fixture extends After { self =>
    Play.start(FakeApplication())

    val claimedUserIds = collection.mutable.Map.empty[EmailAddress, UserId]
    val eventStore: EventStore[UserEvent] = new fake.FakeEventStore

    val memoryImage = MemoryImage[State, UserEvent](eventStore)(State()) {
      (state, commit) => state.updateMany(commit.eventsWithRevision)
    }

    val subject = new UsersController(memoryImage, email => claimedUserIds.getOrElseUpdate(email, UserId.generate()))

    def given(events: UserEvent*)(implicit eventStreamType: EventStreamType[UserId, UserEvent]) {
      for (event <- events) {
        val revision = eventStore.reader.streamRevision(event.userId)
        eventStore.committer.tryCommit(Changes(revision, event)) must beRight
      }
    }

    def users = memoryImage.get.users

    override def after {
      Play.stop()
      eventStore.close
    }
  }
}
