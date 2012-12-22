package eventstore

import java.util.UUID
import play.api.libs.json._
import play.api.libs.json.Json.toJson

object JsonMapping {
  implicit val UuidFormat: Format[UUID] = valueFormat(UUID.fromString)(uuid => Some(uuid.toString))

  // Unsafely extract the results of an unapply method. This makes it easy to directly use the case class generated unapply method.
  private def extract[A, B](o: A, unapply: A => Option[B]): B = unapply(o).getOrElse(throw new IllegalArgumentException("extraction of '" + o + "' failed"))

  /**
   * Define a JSON Format using the provided `reader` and `writer` functions.
   */
  def format[A](reader: JsValue => A)(writer: A => JsValue): Format[A] = new Format[A] {
    override def reads(json: JsValue) = JsSuccess(reader(json))
    override def writes(o: A) = writer(o)
  }

  /**
   * Define a format for a type `A` that wraps a type `B` that already has a `Format`.
   * Useful to convert classes that simply wrap some other type, without adding
   * any additional JSON structure.
   */
  def valueFormat[A, B: Format](apply: B => A)(unapply: A => Option[B]): Format[A] = format(
    json => apply(json.as[B]))(o => toJson(extract(o, unapply)))

  /**
   * Define a JSON Format instance for selecting between multiple sub-types of `R`.
   * There must be no overlap between the different sub-types (none of the provided
   * sub-types can be assignable to another one).
   */
  class TypeChoiceFormat[R](private val choices: Seq[TypeChoiceMapping[_ <: R]]) extends Format[R] {
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

    override def reads(json: JsValue) = {
      val name = (json \ "type").as[String]
      val mapping = choices.find(_.typeName == name).getOrElse(throw new IllegalArgumentException("no mapping for type '" + name + "'"))
      JsSuccess(mapping.fromJson(json \ "data"))
    }
    override def writes(o: R) = {
      val mapping = choices.find(_.matchesInstance(o)).getOrElse(throw new IllegalArgumentException("no mapping found for instance " + o))
      JsObject(Seq("type" -> toJson(mapping.typeName), "data" -> mapping.toJson(o)))
    }
  }
  object TypeChoiceFormat {
    def apply[R](choices: TypeChoiceMapping[_ <: R]*): TypeChoiceFormat[R] = new TypeChoiceFormat[R](choices)
  }

  implicit class TypeChoiceMapping[A](typeAndFormat: (String, Format[A]))(implicit val manifest: Manifest[A]) {
    def typeName = typeAndFormat._1
    def format = typeAndFormat._2
    def erasure: Class[_] = manifest.runtimeClass
    def matchesInstance(o: Any) = manifest.runtimeClass.isInstance(o)
    def fromJson(json: JsValue) = json.as(format)
    def toJson(o: Any) = Json.toJson(o.asInstanceOf[A])(format)
  }

  /**
   * Maps an `Either[A, B]` to a JSON object. When the mapped value is `Left(a)` the JSON object will
   * have a single field named by the `left` parameter. Otherwise the JSON object contains a field
   * named by the `right` parameter.
   */
  def eitherFormat[A: Format, B: Format](left: String, right: String): Format[Either[A, B]] = new Format[Either[A, B]] {
    override def reads(json: JsValue) = (json \ left) match {
      case JsUndefined(_) => JsSuccess(Right((json \ right).as[B]))
      case a              => JsSuccess(Left(a.as[A]))
    }
    override def writes(o: Either[A, B]) = o match {
      case Left(a)  => JsObject(Seq(left -> toJson(a)))
      case Right(b) => JsObject(Seq(right -> toJson(b)))
    }
  }

  /**
   * Maps an instance to a JSON object with a field named `a` using the provided `apply` and `unapply` functions.
   */
  def objectFormat[R, A: Format](a: String)(apply: A => R)(unapply: R => Option[A]): Format[R] = new Format[R] {
    override def reads(json: JsValue) = JsSuccess(apply((json \ a).as[A]))
    override def writes(o: R) = {
      val unapplied = extract(o, unapply)
      JsObject(Seq(a -> toJson(unapplied)))
    }
  }

  /**
   * Maps an instance to a JSON object with a fields named `a` and `b` using the provided `apply` and `unapply` functions.
   */
  def objectFormat[R, A: Format, B: Format](a: String, b: String)(apply: (A, B) => R)(unapply: R => Option[(A, B)]): Format[R] = new Format[R] {
    override def reads(json: JsValue) = JsSuccess(apply((json \ a).as[A], (json \ b).as[B]))
    override def writes(o: R) = {
      val unapplied = extract(o, unapply)
      JsObject(Seq(a -> toJson(unapplied._1), b -> toJson(unapplied._2)))
    }
  }

  /**
   * Maps an instance to a JSON object with a fields named `a`, `b`, and `c` using the provided `apply` and `unapply` functions.
   */
  def objectFormat[R, A: Format, B: Format, C: Format](a: String, b: String, c: String)(apply: (A, B, C) => R)(unapply: R => Option[(A, B, C)]): Format[R] = new Format[R] {
    override def reads(json: JsValue) = JsSuccess(apply((json \ a).as[A], (json \ b).as[B], (json \ c).as[C]))
    override def writes(o: R) = {
      val unapplied = extract(o, unapply)
      JsObject(Seq(a -> toJson(unapplied._1), b -> toJson(unapplied._2), c -> toJson(unapplied._3)))
    }
  }

  /**
   * Maps an instance to a JSON object with a fields named `a`, `b`, `c`, and `d` using the provided `apply` and `unapply` functions.
   */
  def objectFormat[R, A: Format, B: Format, C: Format, D: Format](a: String, b: String, c: String, d: String)(apply: (A, B, C, D) => R)(unapply: R => Option[(A, B, C, D)]): Format[R] = new Format[R] {
    override def reads(json: JsValue) = JsSuccess(apply((json \ a).as[A], (json \ b).as[B], (json \ c).as[C], (json \ d).as[D]))
    override def writes(o: R) = {
      val unapplied = extract(o, unapply)
      JsObject(Seq(a -> toJson(unapplied._1), b -> toJson(unapplied._2), c -> toJson(unapplied._3), d -> toJson(unapplied._4)))
    }
  }

  /**
   * Maps an instance to a JSON object with a fields named `a`, `b`, `c`, `d`, and `e` using the provided `apply` and `unapply` functions.
   */
  def objectFormat[R, A: Format, B: Format, C: Format, D: Format, E: Format](a: String, b: String, c: String, d: String, e: String)(apply: (A, B, C, D, E) => R)(unapply: R => Option[(A, B, C, D, E)]): Format[R] = new Format[R] {
    override def reads(json: JsValue) = JsSuccess(apply((json \ a).as[A], (json \ b).as[B], (json \ c).as[C], (json \ d).as[D], (json \ e).as[E]))
    override def writes(o: R) = {
      val unapplied = extract(o, unapply)
      JsObject(Seq(a -> toJson(unapplied._1), b -> toJson(unapplied._2), c -> toJson(unapplied._3), d -> toJson(unapplied._4), e -> toJson(unapplied._5)))
    }
  }

  /**
   * Maps an instance to a JSON object with a fields named `a`, `b`, `c`, `d`, `e`, and 'f' using the provided `apply` and `unapply` functions.
   */
  def objectFormat[R, A: Format, B: Format, C: Format, D: Format, E: Format, F: Format](a: String, b: String, c: String, d: String, e: String, f: String)(apply: (A, B, C, D, E, F) => R)(unapply: R => Option[(A, B, C, D, E, F)]): Format[R] = new Format[R] {
    override def reads(json: JsValue) = JsSuccess(apply((json \ a).as[A], (json \ b).as[B], (json \ c).as[C], (json \ d).as[D], (json \ e).as[E], (json \ f).as[F]))
    override def writes(o: R) = {
      val unapplied = extract(o, unapply)
      JsObject(Seq(a -> toJson(unapplied._1), b -> toJson(unapplied._2), c -> toJson(unapplied._3), d -> toJson(unapplied._4), e -> toJson(unapplied._5), f -> toJson(unapplied._6)))
    }
  }
}
