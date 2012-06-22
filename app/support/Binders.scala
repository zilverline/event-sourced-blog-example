package support

import java.net.URLDecoder
import java.util.UUID

import play.api.mvc.PathBindable

object Binders {
  implicit object bindableUuid extends PathBindable[UUID] {
    override def bind(key: String, value: String) = try {
      Right(UUID.fromString(URLDecoder.decode(value, "utf-8")))
    } catch {
      case _: Exception => Left("invalid id " + URLDecoder.decode(value, "utf-8"))
    }
    override def unbind(key: String, value: UUID) = value.toString
  }
}
