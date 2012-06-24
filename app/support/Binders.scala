package support

import java.net.URLDecoder
import java.util.UUID

object Binders {
  implicit object pathBindableUuid extends play.api.mvc.PathBindable[UUID] {
    override def bind(key: String, value: String) = try {
      Right(UUID.fromString(URLDecoder.decode(value, "utf-8")))
    } catch {
      case _: Exception => Left("Cannot parse parameter " + key + " as UUID: " + URLDecoder.decode(value, "utf-8"))
    }
    override def unbind(key: String, value: UUID) = value.toString
  }
}
