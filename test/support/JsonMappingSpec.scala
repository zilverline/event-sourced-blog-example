package support

import play.api.libs.json._
import JsonMapping._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class JsonMappingSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  class Parent
  case class Child() extends Parent
  case class Sibling() extends Parent

  val ChildFormat = format[Child](json => new Child)(o => JsString("child"))
  val SiblingFormat = format[Sibling](json => new Sibling)(o => JsString("sibling"))

  implicit val subject: TypeChoiceFormat[Parent] = TypeChoiceFormat("Child" -> ChildFormat, "Sibling" -> SiblingFormat)

  "Choice mapping" should {
    "fail on overlapping choices" in {
      val ParentFormat = format[Parent](json => new Parent)(o => JsString("parent"))
      TypeChoiceFormat("parent" -> ParentFormat, "child" -> ChildFormat) must throwAn[IllegalArgumentException]
    }

    "choose among sub-types when writing" in {
      Json.stringify(Json.toJson(new Child: Parent)) must_== """{"type":"Child","data":"child"}"""
      Json.stringify(Json.toJson(new Sibling: Parent)) must_== """{"type":"Sibling","data":"sibling"}"""
    }

    "choose among sub-types when reading" in {
      Json.fromJson[Parent](Json.parse("""{"type":"Child","data":"child"}""")) must_== Child()
      Json.fromJson[Parent](Json.parse("""{"type":"Sibling","data":"sibling"}""")) must_== Sibling()
    }

    "combine with other mapping" in {
      val OtherFormat = TypeChoiceFormat("String" -> implicitly[Format[String]])
      implicit val CombinedFormat: Format[AnyRef] = subject and OtherFormat

      Json.stringify(Json.toJson(new Child: AnyRef)) must_== """{"type":"Child","data":"child"}"""
      Json.stringify(Json.toJson("string": AnyRef)) must_== """{"type":"String","data":"string"}"""

      Json.fromJson[AnyRef](Json.parse("""{"type":"Sibling","data":"sibling"}""")) must_== Sibling()
      Json.fromJson[AnyRef](Json.parse("""{"type":"String","data":"string"}""")) must_== "string"
    }
  }
}
