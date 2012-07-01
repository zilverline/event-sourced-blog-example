package support

import java.net.URLDecoder
import java.util.UUID

import events.PostId

object Binders {
  implicit object pathBindablePostId extends play.api.mvc.PathBindable[PostId] {
    override def bind(key: String, value: String) = try {
      Right(PostId(UUID.fromString(URLDecoder.decode(value, "utf-8"))))
    } catch {
      case _: RuntimeException => Left("Cannot parse parameter " + key + " as PostId: " + URLDecoder.decode(value, "utf-8"))
    }
    override def unbind(key: String, value: PostId) = value.uuid.toString
  }
}
