package events

import com.lambdaworks.crypto.SCryptUtil
import java.util.UUID
import play.api.libs.json.Format
import support.ConflictsWith
import support.EventStreamType
import support.Identifier
import support.IdentifierCompanion
import support.JsonMapping._

/**
 * Strongly typed identifier for users.
 */
case class UserId(uuid: UUID) extends Identifier
object UserId extends IdentifierCompanion[UserId]("UserId")

case class Email(value: String)
case class Password(hash: String) {
  require(hash.startsWith("$s0$"), "Are you sure you passed in the password hash?")

  def verify(password: String): Boolean = SCryptUtil.check(password, hash)

  override def toString = "Password(<HASH>)"
}
object Password {
  def fromPlainText(password: String) = Password(SCryptUtil.scrypt(password, 1 << 14, 8, 2))
}

sealed trait UserEvent extends DomainEvent {
  def userId: UserId
}
case class UserRegistered(userId: UserId, login: Email, password: Password) extends UserEvent
case class UserPasswordChanged(userId: UserId, password: Password) extends UserEvent

object UserEvent {
  implicit val UserEventDescriptor: EventStreamType[UserId, UserEvent] = EventStreamType(_.toString, _.userId)
  implicit val UserEventConflictsWith: ConflictsWith[UserEvent] = ConflictsWith { case _ => true }

  implicit val EmailFormat: Format[Email] = valueFormat(Email.apply)(Email.unapply)
  implicit val PasswordFormat: Format[Password] = valueFormat(Password.apply)(Password.unapply)

  implicit val UserEventFormat: TypeChoiceFormat[UserEvent] = TypeChoiceFormat(
    "UserRegistered" -> objectFormat("userId", "login", "password")(UserRegistered.apply)(UserRegistered.unapply),
    "UserPasswordChanged" -> objectFormat("userId", "password")(UserPasswordChanged.apply)(UserPasswordChanged.unapply))
}
