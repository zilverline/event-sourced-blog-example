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

  implicit val subject: Format[Parent] = typeChoiceFormat("Child" -> ChildFormat, "Sibling" -> SiblingFormat)

  "Choice mapping" should {
    "fail on overlapping choices" in {
      val ParentFormat = format[Parent](json => new Parent)(o => JsString("parent"))
      typeChoiceFormat("parent" -> ParentFormat, "child" -> ChildFormat) must throwAn[IllegalArgumentException]
    }

    "choose among sub-types when writing" in {
      Json.stringify(Json.toJson(new Child: Parent)) must_== """{"type":"Child","data":"child"}"""
      Json.stringify(Json.toJson(new Sibling: Parent)) must_== """{"type":"Sibling","data":"sibling"}"""
    }

    "choose among sub-types when reading" in {
      Json.fromJson[Parent](Json.parse("""{"type":"Child","data":"child"}""")) must_== Child()
      Json.fromJson[Parent](Json.parse("""{"type":"Sibling","data":"sibling"}""")) must_== Sibling()
    }
  }
}
