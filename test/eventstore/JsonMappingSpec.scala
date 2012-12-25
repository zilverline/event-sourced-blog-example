package eventstore

import JsonMapping._
import play.api.libs.json._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class JsonMappingSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  class Parent
  case class Child() extends Parent
  case class Sibling() extends Parent

  val ChildFormat = Format[Child](Reads(json => JsSuccess(new Child)), Writes(o => JsString("child")))
  val SiblingFormat = Format[Sibling](Reads(json => JsSuccess(new Sibling)), Writes(o => JsString("sibling")))
  val OtherFormat = TypeChoiceFormat("String" -> implicitly[Format[String]])

  implicit val subject: TypeChoiceFormat[Parent] = TypeChoiceFormat("Child" -> ChildFormat, "Sibling" -> SiblingFormat)

  "Choice mapping" should {
    "fail on overlapping choices" in {
      val ParentFormat = Format[Parent](Reads(json => JsSuccess(new Parent)), Writes(o => JsString("parent")))
      TypeChoiceFormat("parent" -> ParentFormat, "child" -> ChildFormat) must throwAn[IllegalArgumentException]
    }

    "choose among sub-types when writing" in {
      Json.stringify(Json.toJson(new Child: Parent)) must_== """{"type":"Child","data":"child"}"""
      Json.stringify(Json.toJson(new Sibling: Parent)) must_== """{"type":"Sibling","data":"sibling"}"""
    }

    "choose among sub-types when reading" in {
      Json.fromJson[Parent](Json.parse("""{"type":"Child","data":"child"}""")) must_== JsSuccess(Child())
      Json.fromJson[Parent](Json.parse("""{"type":"Sibling","data":"sibling"}""")) must_== JsSuccess(Sibling())
    }

    "combine with other mapping" in {
      implicit val CombinedFormat: Format[AnyRef] = subject and OtherFormat

      Json.stringify(Json.toJson(new Child: AnyRef)) must_== """{"type":"Child","data":"child"}"""
      Json.stringify(Json.toJson("string": AnyRef)) must_== """{"type":"String","data":"string"}"""

      Json.fromJson[AnyRef](Json.parse("""{"type":"Sibling","data":"sibling"}""")) must_== JsSuccess(Sibling())
      Json.fromJson[AnyRef](Json.parse("""{"type":"String","data":"string"}""")) must_== JsSuccess("string")
    }
  }

  "Either format" should {
    val eitherIntOrString = eitherFormat[Int, String]("int", "string")

    "read left side if present" in prop { (i: Int) =>
      eitherIntOrString.reads(Json.obj("int" -> i)) must_== JsSuccess(Left(i), __ \ "int")
    }

    "read right side if present" in prop { (s: String) =>
      eitherIntOrString.reads(Json.obj("string" -> s)) must_== JsSuccess(Right(s), __ \ "string")
    }

    "fail to read if neither field is present" in {
      eitherIntOrString.reads(Json.obj()) must beAnInstanceOf[JsError]
    }

    "write left side" in prop { (i: Int) =>
      eitherIntOrString.writes(Left(i)) must_== Json.obj("int" -> i)
    }

    "write right side" in prop { (s: String) =>
      eitherIntOrString.writes(Right(s)) must_== Json.obj("string" -> s)
    }
  }
}
