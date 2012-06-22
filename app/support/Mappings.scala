package support

import java.util.UUID

import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._

object Mappings {
  def isUuid(s: String): Boolean = try { UUID.fromString(s); true } catch { case _: Exception => false }

  val trimmedText = text.transform[String](_.trim, identity)
  val uuid = text.verifying(isUuid _).transform[UUID](UUID.fromString, _.toString)
}
