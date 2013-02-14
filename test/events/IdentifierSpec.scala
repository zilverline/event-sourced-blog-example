package events

import java.util.UUID
import org.scalacheck._, Arbitrary.arbitrary
import play.api.libs.json._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class IdentifierSpec extends support.Spec {
  import IdentifierSpec._

  case class Id(uuid: UUID) extends Identifier
  object Id extends IdentifierCompanion[Id]("Id")

  "An identifier" should {
    "be equal to itself" in prop { (id: Id) =>
      id must_== id
    }

    "generate unique values" in prop { (a: Id, b: Id) =>
      a must_!= b
    }

    "convert to and from Strings" in prop { (id: Id) =>
      Id.fromString(id.toString) must beSome(id)
    }

    "convert to and from JSON" in prop { (id: Id) =>
      Json.fromJson[Id](Json.toJson(id)) must_== JsSuccess(id)
    }

    "fail to parse invalid strings" in prop { (s: String) =>
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
