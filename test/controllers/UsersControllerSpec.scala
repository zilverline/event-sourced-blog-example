package controllers

import play.api.test._
import play.api.test.Helpers._
import events._
import eventstore._
import models._

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

      val response = subject.register(request)

      status(response) must_== 303
      changes must have size 1
      changes(0) must beAnInstanceOf[UserRegistered]

      val event = changes(0).asInstanceOf[UserRegistered]
      event.email must_== email
      event.password.verify("password") aka "password verified" must beTrue
      event.userId aka "claimed user id" must_== userId
    }

    "allow registered user to log in" in new fixture {
      given(UserRegistered(userId, email, displayName, password): UserEvent)

      val response = subject.logIn(FakeRequest().withFormUrlEncodedBody("email" -> email.value, "password" -> "password"))

      status(response) must_== 303
      val token = session(response).get("authenticationToken").flatMap(AuthenticationToken.fromString) getOrElse { failure("authentication token not created") }
      changes must_== Seq(UserLoggedIn(userId, token))
    }

    "allow logged in users to log out" in new fixture {
      given(
        UserRegistered(userId, email, displayName, password): UserEvent,
        UserLoggedIn(userId, authenticationToken): UserEvent)

      val response = subject.logOut(authenticated)

      status(response) must_== 303
      session(response) must beEmpty
      changes must_== Seq(UserLoggedOut(userId))
    }

    "allow editing the user's profile" in new fixture {
      given(
        UserRegistered(userId, email, displayName, password): UserEvent,
        UserLoggedIn(userId, authenticationToken): UserEvent)

      val response = subject.changeProfile(authenticated.withFormUrlEncodedBody("displayName" -> "Updated"))

      status(response) must_== 303
      changes must_== Seq(UserProfileChanged(userId, displayName = "Updated"))
    }

    "allow changing the user's email" in new fixture {
      val updated = EmailAddress("updated@example.com")
      given(
        UserRegistered(userId, email, displayName, password): UserEvent,
        UserLoggedIn(userId, authenticationToken): UserEvent)

      val response = subject.changeEmailAddress(authenticated.withFormUrlEncodedBody("email" -> updated.value))

      status(response) must_== 303
      changes must_== Seq(UserEmailAddressChanged(userId, EmailAddress("updated@example.com")))
      claimedUserIds must havePair(updated -> userId)
    }
  }

  trait fixture extends ControllerFixture {
    val claimedUserIds = collection.mutable.Map.empty[EmailAddress, UserId]

    val authenticated = FakeRequest().withSession("authenticationToken" -> authenticationToken.toString)

    val subject = new UsersController(
      new MemoryImageActions(memoryImage).view(_.users),
      (email, userId) => claimedUserIds.getOrElseUpdate(email, userId))
  }
}
