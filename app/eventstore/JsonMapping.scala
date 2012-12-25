package eventstore

import java.util.UUID
import play.api.libs.functional.syntax._
import play.api.libs.json._

object JsonMapping {
  implicit val UuidFormat: Format[UUID] = valueFormat(UUID.fromString)(_.toString)

  /**
   * Define a format for a type `A` that wraps a type `B` that already has a `Format`.
   * Useful to convert classes that simply wrap some other type, without adding
   * any additional JSON structure.
   */
  def valueFormat[A, B: Format](apply: B => A)(unapply: A => B): Format[A] = Format(
    Reads.of[B].map(apply), Writes(a => Writes.of[B].writes(unapply(a))))

  /**
   * Define a JSON Format instance for selecting between multiple sub-types of `R`.
   * There must be no overlap between the different sub-types (none of the provided
   * sub-types can be assignable to another one).
   */
  class TypeChoiceFormat[R](private val choices: Seq[TypeChoiceMapping[_ <: R]]) extends OFormat[R] {
    {
      val typeNames = choices.map(_.typeName)
      require(typeNames == typeNames.distinct, "duplicate type names in: " + typeNames.mkString(", "))
      val overlapping = choices.combinations(2).filter(pair => pair(0).erasure isAssignableFrom pair(1).erasure)
      require(overlapping.isEmpty, "overlapping choices are not allowed: " + overlapping.map(pair => pair(0).manifest + " is assignable from " + pair(1).manifest).mkString(", "))
    }

    /**
     * Combine two type choice formats.
     */
    def and[RR >: R](that: TypeChoiceFormat[_ <: RR]): TypeChoiceFormat[RR] =
      new TypeChoiceFormat[RR]((this.choices: Seq[TypeChoiceMapping[_ <: RR]]) ++ (that.choices: Seq[TypeChoiceMapping[_ <: RR]]))

    override def reads(json: JsValue) = for {
      (name, data) <- ((JsPath \ "type").read[String] and (JsPath \ "data").read[JsValue]).tupled.reads(json)
      mapping <- choices.find(_.typeName == name).map(mapping => JsSuccess[TypeChoiceMapping[_ <: R]](mapping)).getOrElse(JsError(s"mapping '$name' not in ${choices.map(_.typeName).mkString("[", ", ", "]")}"))
      parsed <- mapping.fromJson(data)
    } yield parsed
    override def writes(o: R) = {
      val mapping = choices.find(_.matchesInstance(o)).getOrElse(throw new IllegalArgumentException("no mapping found for instance " + o))
      Json.obj("type" -> mapping.typeName, "data" -> mapping.toJson(o))
    }
  }
  object TypeChoiceFormat {
    def apply[R](choices: TypeChoiceMapping[_ <: R]*): TypeChoiceFormat[R] = new TypeChoiceFormat[R](choices)
  }

  implicit class TypeChoiceMapping[A](typeAndFormat: (String, Format[A]))(implicit val manifest: Manifest[A]) {
    def typeName = typeAndFormat._1
    def format = typeAndFormat._2
    def erasure: Class[_] = manifest.runtimeClass
    def matchesInstance(o: Any): Boolean = manifest.runtimeClass.isInstance(o)
    def fromJson(json: JsValue): JsResult[A] = format.reads(json)
    def toJson(o: Any): JsValue = format.writes(o.asInstanceOf[A])
  }

  /**
   * Maps an `Either[A, B]` to a JSON object. When the mapped value is `Left(a)` the JSON object will
   * have a single field named by the `left` parameter. Otherwise the JSON object contains a field
   * named by the `right` parameter.
   */
  def eitherFormat[A: Format, B: Format](left: String, right: String): OFormat[Either[A, B]] = new OFormat[Either[A, B]] {
    override def reads(json: JsValue) = (
      (__ \ left).read[A].map(a => Left(a): Either[A, B]) orElse
      (__ \ right).read[B].map(b => Right(b): Either[A, B])
    ).reads(json)
    override def writes(o: Either[A, B]) = o match {
      case Left(a)  => Json.obj(left -> a)
      case Right(b) => Json.obj(right -> b)
    }
  }
}
