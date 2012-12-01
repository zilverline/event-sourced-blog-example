package events

import eventstore._
import org.scalacheck._, Arbitrary.arbitrary, Prop.forAll
import play.api.libs.json._

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

    "convert to and from JSON" in {
      Json.fromJson[Password](Json.toJson(password)) must_== password
    }
  }

  "An authentication token" should {
    "be equal to itself" in forAll { (id: AuthenticationToken) =>
      id must_== id
    }

    "generate unique values" in forAll { (a: AuthenticationToken, b: AuthenticationToken) =>
      a must_!= b
    }

    "convert to and from Strings" in forAll { (id: AuthenticationToken) =>
      AuthenticationToken.fromString(id.toString) must beSome(id)
    }

    "convert to and from JSON" in forAll { (id: AuthenticationToken) =>
      Json.fromJson[AuthenticationToken](Json.toJson(id)) must_== id
    }

    "fail to parse invalid strings" in forAll { (s: String) =>
      AuthenticationToken.fromString(s) match {
        case Some(token) => token.toString must_== s
        case None        => ok
      }
    }
  }
}

object UserEventsSpec {
  implicit val arbitraryAuthenticationToken: Arbitrary[AuthenticationToken] = Arbitrary(Gen.wrap(AuthenticationToken.generate()))
}
