package events

import eventstore._
import org.scalacheck._, Arbitrary.arbitrary
import play.api.libs.json._
import IdentifierSpec._
import Generators._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class UserEventsSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  import UserEventsSpec._
  import UserEvent._

  "A password" should {
    val password = Password.fromPlainText("password")

    "verify against plain text" in {
      password.verify("password") aka "correct password" must beTrue
      password.verify("different") aka "different password" must beFalse
    }

    "hide the internal contents when printed" in {
      password.toString must_== "<PASSWORD-HASH>"
    }

    "not simply store the password itself" in {
      password.hash must_!= "password"
    }

    "convert to and from JSON" in prop { (password: Password) =>
      Json.fromJson[Password](Json.toJson(password)) must_== password
    }
  }

  "An authentication token" should {
    "be equal to itself" in prop { (token: AuthenticationToken) =>
      token must_== token
    }

    "generate unique values" in prop { (a: AuthenticationToken, b: AuthenticationToken) =>
      a must_!= b
    }

    "convert to and from Strings" in prop { (token: AuthenticationToken) =>
      AuthenticationToken.fromString(token.toString) must beSome(token)
    }

    "convert to and from JSON" in prop { (token: AuthenticationToken) =>
      Json.fromJson[AuthenticationToken](Json.toJson(token)) must_== token
    }

    "fail to parse invalid strings" in prop { (s: String) =>
      AuthenticationToken.fromString(s) match {
        case Some(token) => token.toString must_== s
        case None        => ok
      }
    }
  }

  "User events" should {
    "convert to and from JSON" in eventsForMultipleUsers { events =>
      Json.fromJson[List[UserEvent]](Json.toJson(events)) must_== events
    }
  }
}

object UserEventsSpec {
  implicit val arbitraryAuthenticationToken: Arbitrary[AuthenticationToken] = Arbitrary(Gen.wrap(AuthenticationToken.generate()))
  implicit val arbitraryEmailAddress: Arbitrary[EmailAddress] = Arbitrary(Gen.oneOf(Seq("john@example.com", "jane@example.com").map(EmailAddress.apply)))
  implicit val arbitraryPassword: Arbitrary[Password] = Arbitrary(Gen.oneOf(Seq("password", "secret", "12345").map(Password.fromPlainText)))

  implicit val shrinkUserEvents: Shrink[List[UserEvent]] = Shrink(_ => Stream.empty)

  private def loginWithOptionalLogout(id: UserId) = for {
    login <- Gen.resultOf(UserLoggedIn(id, _: AuthenticationToken))
    logout <- Gen.frequency(2 -> Nil, 1 -> (UserLoggedOut(id) :: Nil))
  } yield login :: logout

  def eventsForSingleUser(id: UserId): Arbitrary[List[UserEvent]] = Arbitrary(for {
    registered <- Gen.resultOf(UserRegistered(id, _: EmailAddress, _: String, _: Password))
    profileChanges <- Gen.resize(2, Gen.listOf(Gen.resultOf(UserProfileChanged(id, _: String))))
    emailChanges <- Gen.resize(2, Gen.listOf(Gen.resultOf(UserEmailAddressChanged(id, _: EmailAddress))))
    passwordChanges <- Gen.resize(2, Gen.listOf(Gen.resultOf(UserPasswordChanged(id, _: Password))))
    logins <- Gen.resize(5, Gen.listOf(loginWithOptionalLogout(id)).map(_.flatten))
    events <- interleaved(profileChanges, emailChanges, passwordChanges, logins)
  } yield registered :: events)

  val eventsForMultipleUsers: Arbitrary[List[UserEvent]] = Arbitrary(for {
    events <- Gen.resize(10, Gen.listOf(arbitrary[UserId].flatMap { id => Gen.resize(5, arbitrary(eventsForSingleUser(id))) }))
    x <- interleaved(events: _*)
  } yield x)
}
