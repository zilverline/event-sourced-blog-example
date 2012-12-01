package events

import java.util.UUID
import org.scalacheck._, Arbitrary.arbitrary, Prop.forAll
import play.api.libs.json._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class IdentifierSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  import IdentifierSpec._

  case class Id(uuid: UUID) extends Identifier
  object Id extends IdentifierCompanion[Id]("Id")

  "An identifier" should {
    "be equal to itself" in forAll { (id: Id) =>
      id must_== id
    }

    "generate unique values" in forAll { (a: Id, b: Id) =>
      a must_!= b
    }

    "convert to and from Strings" in forAll { (id: Id) =>
      Id.fromString(id.toString) must beSome(id)
    }

    "convert to and from JSON" in forAll { (id: Id) =>
      Json.fromJson[Id](Json.toJson(id)) must_== id
    }

    "fail to parse invalid strings" in forAll { (s: String) =>
      Id.fromString(s) match {
        case Some(id) => id.toString must_== s
        case None     => ok
      }
    }
  }
}
object IdentifierSpec {
  implicit def ArbitraryIdentifier[A <: Identifier](implicit companion: IdentifierCompanion[A]): Arbitrary[A] = Arbitrary(Gen.wrap(companion.generate()))
}
