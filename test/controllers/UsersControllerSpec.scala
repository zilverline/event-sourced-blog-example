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
  val displayName = "John Doe"
  val password = Password.fromPlainText("password")
  val authenticationToken = AuthenticationToken.generate

  "users controller" should {
    "register a new user" in new fixture {
      claimedUserIds += email -> userId
      val request = FakeRequest().withFormUrlEncodedBody("email" -> "john@example.com", "displayName" -> "John Doe", "password.1" -> "password", "password.2" -> "password")

      val result = subject.register.submit(request)

      status(result) must_== 303
      changes must have size(1)
      changes(0) must beAnInstanceOf[UserRegistered]

      val event = changes(0).asInstanceOf[UserRegistered]
      event.login must_== EmailAddress("john@example.com")
      event.password.verify("password") aka "password verified" must beTrue
      event.userId aka "claimed user id" must_== userId
    }

    "allow registered user to log in" in new fixture {
      given(UserRegistered(userId, email, displayName, password): UserEvent)

      val result = subject.authentication.submit(FakeRequest().withFormUrlEncodedBody("email" -> email.value, "password" -> "password"))

      status(result) must_== 303
      val token = session(result).get("authenticationToken").flatMap(AuthenticationToken.fromString) getOrElse { failure("authentication token not created") }
      changes must_== Seq(UserLoggedIn(userId, token))
    }

    "allow logged in users to log out" in new fixture {
      given(
          UserRegistered(userId, email, displayName, password): UserEvent,
          UserLoggedIn(userId, authenticationToken): UserEvent)

      val result = subject.authentication.logOut(FakeRequest().withSession("authenticationToken" -> authenticationToken.toString))

      status(result) must_== 303
      session(result) must beEmpty
      changes must_== Seq(UserLoggedOut(userId))
    }
  }

  trait fixture extends ControllerFixture {
    val claimedUserIds = collection.mutable.Map.empty[EmailAddress, UserId]

    val subject = new UsersController(new MemoryImageActions(memoryImage).view(_.users), email => claimedUserIds.getOrElseUpdate(email, UserId.generate()))
  }
}
