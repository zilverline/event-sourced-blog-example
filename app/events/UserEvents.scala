package events

import com.lambdaworks.crypto.SCryptUtil
import eventstore.ConflictsWith
import eventstore.EventStreamType
import eventstore.JsonMapping._
import java.util.UUID
import play.api.libs.json.Format

/**
 * Strongly typed identifier for users.
 */
case class UserId(uuid: UUID) extends Identifier
object UserId extends IdentifierCompanion[UserId]("UserId")

case class EmailAddress(value: String) {
  require(value.matches(".+@.+"), "not a valid email address: " + value)

  override def toString = value
}
object EmailAddress {
  implicit val EmailAddressFormat: Format[EmailAddress] = valueFormat(EmailAddress.apply)(EmailAddress.unapply)
}

case class Password private (hash: String) {
  require(hash.startsWith("$s0$"), "Are you sure you passed in the password hash?")

  def verify(password: String): Boolean = SCryptUtil.check(password, hash)

  override def toString = "<PASSWORD-HASH>"
}
object Password {
  def fromHash(hash: String): Password = Password(hash)
  def fromPlainText(password: String): Password = Password(SCryptUtil.scrypt(password, 1 << 14, 8, 2))

  implicit val PasswordFormat: Format[Password] = valueFormat(Password.fromHash)(Password.unapply)
}

case class AuthenticationToken private (a: Long, b: Long) {
  override def toString = "%016x-%016x".format(a, b)
}
object AuthenticationToken {
  def generate(): AuthenticationToken = AuthenticationToken(Random.nextLong, Random.nextLong)

  def apply(s: String): AuthenticationToken = fromString(s) getOrElse {
    throw new IllegalArgumentException("invalid authentication token '%s'".format(s))
  }
  def fromString(s: String): Option[AuthenticationToken] = s match {
    case Pattern(a, b) => Some(AuthenticationToken(BigInt(a, 16).longValue, BigInt(b, 16).longValue))
    case _             => None
  }

  implicit val AuthenticationTokenFormat: Format[AuthenticationToken] = valueFormat(AuthenticationToken.apply)(x => Some(x.toString))

  private[this] val Pattern = """\b([0-9a-fA-F]{16})-([0-9a-fA-F]{16})\b""".r
  private[this] val Random = new java.security.SecureRandom()
}

sealed trait UserEvent extends DomainEvent {
  def userId: UserId
}
case class UserRegistered(userId: UserId, login: EmailAddress, password: Password) extends UserEvent
case class UserPasswordChanged(userId: UserId, password: Password) extends UserEvent
case class UserLoggedIn(userId: UserId, token: AuthenticationToken) extends UserEvent
case class UserLoggedOut(userId: UserId) extends UserEvent

object UserEvent {
  implicit val UserEventDescriptor: EventStreamType[UserId, UserEvent] = EventStreamType(_.toString, _.userId)
  implicit val UserEventConflictsWith: ConflictsWith[UserEvent] = ConflictsWith { case _ => true }

  implicit val UserEventFormat: TypeChoiceFormat[UserEvent] = TypeChoiceFormat(
    "UserRegistered" -> objectFormat("userId", "login", "password")(UserRegistered.apply)(UserRegistered.unapply),
    "UserPasswordChanged" -> objectFormat("userId", "password")(UserPasswordChanged.apply)(UserPasswordChanged.unapply),
    "UserLoggedIn" -> objectFormat("userId", "authenticationToken")(UserLoggedIn.apply)(UserLoggedIn.unapply),
    "UserLoggedOut" -> objectFormat("userId")(UserLoggedOut.apply)(UserLoggedOut.unapply))
}
