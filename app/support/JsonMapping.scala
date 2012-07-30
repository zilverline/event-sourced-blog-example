package support

import java.util.UUID
import play.api.libs.json._, Json.toJson

object JsonMapping {
  implicit val UuidFormat: Format[UUID] = valueFormat(UUID.fromString)(uuid => Some(uuid.toString))

  // Unsafely extract the results of an unapply method. This makes it easy to directly use the case class generated unapply method.
  private def extract[A, B](o: A, unapply: A => Option[B]): B = unapply(o).getOrElse(throw new IllegalArgumentException("extraction of '" + o + "' failed"))

  /**
   * Define a JSON Format using the provided `reader` and `writer` functions.
   */
  def format[A](reader: JsValue => A)(writer: A => JsValue): Format[A] = new Format[A] {
    override def reads(json: JsValue) = reader(json)
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
  def typeChoiceFormat[R](choices: TypeChoiceMapping[_ <: R]*): Format[R] = new Format[R] {
    {
      val overlapping = choices.combinations(2).filter(pair => pair(0).erasure isAssignableFrom pair(1).erasure)
      require(overlapping.isEmpty, "overlapping choices are not allowed: " + overlapping.map(pair => pair(0).manifest + " is assignable from " + pair(1).manifest).mkString(", "))
    }

    override def reads(json: JsValue) = {
      val name = (json \ "type").as[String]
      val mapping = choices.find(_.typeName == name).getOrElse(throw new IllegalArgumentException("no mapping for type '" + name + "'"))
      mapping.fromJson(json \ "data")
    }
    override def writes(o: R) = {
      val mapping = choices.find(_.matchesInstance(o)).getOrElse(throw new IllegalArgumentException("no mapping found for instance " + o))
      JsObject(Seq("type" -> toJson(mapping.typeName), "data" -> mapping.toJson(o)))
    }
  }

  case class TypeChoiceMapping[A](typeName: String, format: Format[A], manifest: Manifest[A]) {
    def erasure: Class[_] = manifest.erasure
    def matchesInstance(o: Any) = manifest.erasure.isInstance(o)
    def fromJson(json: JsValue) = json.as(format)
    def toJson(o: Any) = Json.toJson(o.asInstanceOf[A])(format)
  }
  implicit def typeAndFormatToChoiceMapping[A](typeAndFormat: (String, Format[A]))(implicit m: Manifest[A]): TypeChoiceMapping[A] = TypeChoiceMapping(typeAndFormat._1, typeAndFormat._2, m)

  /**
   * Maps an instance to a JSON object with a field named `a` using the provided `apply` and `unapply` functions.
   */
  def objectFormat[R, A: Format](a: String)(apply: A => R)(unapply: R => Option[A]): Format[R] = new Format[R] {
    override def reads(json: JsValue) = apply((json \ a).as[A])
    override def writes(o: R) = {
      val unapplied = extract(o, unapply)
      JsObject(Seq(a -> toJson(unapplied)))
    }
  }

  /**
   * Maps an instance to a JSON object with a fields named `a` and `b` using the provided `apply` and `unapply` functions.
   */
  def objectFormat[R, A: Format, B: Format](a: String, b: String)(apply: (A, B) => R)(unapply: R => Option[(A, B)]): Format[R] = new Format[R] {
    override def reads(json: JsValue) = apply((json \ a).as[A], (json \ b).as[B])
    override def writes(o: R) = {
      val unapplied = extract(o, unapply)
      JsObject(Seq(a -> toJson(unapplied._1), b -> toJson(unapplied._2)))
    }
  }

  /**
   * Maps an instance to a JSON object with a fields named `a`, `b`, and `c` using the provided `apply` and `unapply` functions.
   */
  def objectFormat[R, A: Format, B: Format, C: Format](a: String, b: String, c: String)(apply: (A, B, C) => R)(unapply: R => Option[(A, B, C)]): Format[R] = new Format[R] {
    override def reads(json: JsValue) = apply((json \ a).as[A], (json \ b).as[B], (json \ c).as[C])
    override def writes(o: R) = {
      val unapplied = extract(o, unapply)
      JsObject(Seq(a -> toJson(unapplied._1), b -> toJson(unapplied._2), c -> toJson(unapplied._3)))
    }
  }

  /**
   * Maps an instance to a JSON object with a fields named `a`, `b`, `c`, and `d` using the provided `apply` and `unapply` functions.
   */
  def objectFormat[R, A: Format, B: Format, C: Format, D: Format](a: String, b: String, c: String, d: String)(apply: (A, B, C, D) => R)(unapply: R => Option[(A, B, C, D)]): Format[R] = new Format[R] {
    override def reads(json: JsValue) = apply((json \ a).as[A], (json \ b).as[B], (json \ c).as[C], (json \ d).as[D])
    override def writes(o: R) = {
      val unapplied = extract(o, unapply)
      JsObject(Seq(a -> toJson(unapplied._1), b -> toJson(unapplied._2), c -> toJson(unapplied._3), d -> toJson(unapplied._4)))
    }
  }

  /**
   * Maps an instance to a JSON object with a fields named `a`, `b`, `c`, `d`, and `e` using the provided `apply` and `unapply` functions.
   */
  def objectFormat[R, A: Format, B: Format, C: Format, D: Format, E: Format](a: String, b: String, c: String, d: String, e: String)(apply: (A, B, C, D, E) => R)(unapply: R => Option[(A, B, C, D, E)]): Format[R] = new Format[R] {
    override def reads(json: JsValue) = apply((json \ a).as[A], (json \ b).as[B], (json \ c).as[C], (json \ d).as[D], (json \ e).as[E])
    override def writes(o: R) = {
      val unapplied = extract(o, unapply)
      JsObject(Seq(a -> toJson(unapplied._1), b -> toJson(unapplied._2), c -> toJson(unapplied._3), d -> toJson(unapplied._4), e -> toJson(unapplied._5)))
    }
  }
}
