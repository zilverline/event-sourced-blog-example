package events

import com.lambdaworks.crypto.SCryptUtil
import eventstore.ConflictsWith
import eventstore.EventStreamType
import eventstore.JsonMapping._
import java.util.UUID
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scala.util.control.Exception.catching

/**
 * Strongly typed identifier for users.
 */
case class UserId(uuid: UUID) extends Identifier
object UserId extends IdentifierCompanion[UserId]("UserId")

case class EmailAddress(value: String) {
  // Pattern copied from play.api.data.Forms.email.
  require(
    value matches """\b[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\b""",
    s"invalid email address: $value")

  override def toString = value
}
object EmailAddress {
  def fromString(s: String): Option[EmailAddress] = catching(classOf[IllegalArgumentException]) opt EmailAddress(s)

  implicit val EmailAddressFormat: Format[EmailAddress] = valueFormat(apply)(_.value)
}

/**
 * Hashed passwords that avoid leaking the contents when converted to strings.
 */
case class Password private (hash: String) {
  require(hash.startsWith("$s0$"), "invalid password hash")

  def verify(password: String): Boolean = SCryptUtil.check(password, hash)

  override def toString = "<PASSWORD-HASH>"
}
object Password {
  def fromHash(hash: String): Password = Password(hash)
  def fromPlainText(password: String): Password = Password(SCryptUtil.scrypt(password, 1 << 14, 8, 2))

  implicit val PasswordFormat: Format[Password] = valueFormat(fromHash)(_.hash)
}

/**
 * Authentication tokens used to track logged-in users. Represented as 128-bit randomly generated numbers.
 */
case class AuthenticationToken private (a: Long, b: Long) {
  override def toString = f"$a%016x-$b%016x"
}
object AuthenticationToken {
  def generate(): AuthenticationToken = AuthenticationToken(Random.nextLong, Random.nextLong)

  def apply(value: String): AuthenticationToken = fromString(value) getOrElse {
    throw new IllegalArgumentException(s"invalid authentication token '$value'")
  }
  def fromString(s: String): Option[AuthenticationToken] = s match {
    case Pattern(a, b) => Some(AuthenticationToken(BigInt(a, 16).longValue, BigInt(b, 16).longValue))
    case _             => None
  }

  implicit val AuthenticationTokenFormat: Format[AuthenticationToken] = valueFormat(AuthenticationToken.apply)(_.toString)

  private val Pattern = """\b([0-9a-fA-F]{16})-([0-9a-fA-F]{16})\b""".r
  private val Random = new java.security.SecureRandom()
}

/**
 * User events.
 */
sealed trait UserEvent extends DomainEvent {
  def userId: UserId
}
case class UserRegistered(userId: UserId, email: EmailAddress, displayName: String, password: Password) extends UserEvent
case class UserProfileChanged(userId: UserId, displayName: String) extends UserEvent
case class UserEmailAddressChanged(userId: UserId, email: EmailAddress) extends UserEvent
case class UserPasswordChanged(userId: UserId, password: Password) extends UserEvent
case class UserLoggedIn(userId: UserId, token: AuthenticationToken) extends UserEvent
case class UserLoggedOut(userId: UserId) extends UserEvent

object UserEvent {
  implicit val UserEventDescriptor: EventStreamType[UserId, UserEvent] = EventStreamType(_.toString, _.userId)
  implicit val UserEventConflictsWith: ConflictsWith[UserEvent] = ConflictsWith {
    case (_: UserRegistered, _) => true
    case _                      => false
  }

  implicit val UserEventFormat: TypeChoiceFormat[UserEvent] = TypeChoiceFormat(
    "UserRegistered"          -> Json.format[UserRegistered],
    "UserProfileChanged"      -> Json.format[UserProfileChanged],
    "UserEmailAddressChanged" -> Json.format[UserEmailAddressChanged],
    "UserPasswordChanged"     -> Json.format[UserPasswordChanged],
    "UserLoggedIn"            -> Json.format[UserLoggedIn],
    "UserLoggedOut"           -> Json.format[UserLoggedOut])
}
