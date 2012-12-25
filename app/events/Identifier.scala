package events

import eventstore.JsonMapping._
import java.util.UUID
import java.util.regex.Pattern
import play.api.libs.json._
import scala.util.control.Exception.catching

trait Identifier {
  def uuid: UUID
}
abstract class IdentifierCompanion[A <: Identifier](val prefix: String) {
  def apply(uuid: UUID): A

  def generate(): A = apply(UUID.randomUUID)

  def fromString(s: String): Option[A] = s match {
    case IdentifierRegex(uuid) => catching(classOf[RuntimeException]) opt { apply(UUID.fromString(uuid)) }
    case _                     => None
  }

  implicit val IdentifierFormat: Format[A] = valueFormat(apply)(_.uuid)

  implicit val IdentifierCompanionObject: IdentifierCompanion[A] = this

  private val IdentifierRegex = (Pattern.quote(prefix) + """\(([a-fA-F0-9-]{36})\)""").r
}
