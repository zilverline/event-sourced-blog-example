package support

import java.net.URLDecoder
import events.PostId

object Binders {
  implicit object pathBindablePostId extends play.api.mvc.PathBindable[PostId] {
    override def bind(key: String, value: String) = try {
      Right(PostId(URLDecoder.decode(value, "utf-8")))
    } catch {
      case _: Exception => Left("Cannot parse parameter " + key + " as UUID: " + URLDecoder.decode(value, "utf-8"))
    }
    override def unbind(key: String, value: PostId) = value.uuid.toString
  }
}
