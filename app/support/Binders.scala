package support

import java.net.URLDecoder
import java.util.UUID
import events.PostId
import eventstore.{ StoreRevision, StreamRevision }

object Binders {
  implicit object pathBindablePostId extends play.api.mvc.PathBindable[PostId] {
    override def bind(key: String, value: String) = try {
      Right(PostId(UUID.fromString(URLDecoder.decode(value, "utf-8"))))
    } catch {
      case _: RuntimeException => Left("Cannot parse parameter " + key + " as PostId: " + URLDecoder.decode(value, "utf-8"))
    }
    override def unbind(key: String, value: PostId) = value.uuid.toString
  }

  implicit object queryStringBindableStoreRevision extends play.api.mvc.QueryStringBindable[StoreRevision] {
    override def bind(key: String, params: Map[String, Seq[String]]) = params.get(key).flatMap(_.headOption).map { value =>
      try {
        Right(StoreRevision(value.toLong))
      } catch {
        case _: RuntimeException => Left("Cannot parse parameter " + key + " as StoreRevision: " + value)
      }
    }
    override def unbind(key: String, value: StoreRevision) = key + "=" + value.value
  }
  implicit def litteralStoreRevision = new play.api.mvc.JavascriptLitteral[StoreRevision] {
    def to(value: StoreRevision) = value.value.toString
  }

  implicit object queryStringindableStreamRevision extends play.api.mvc.QueryStringBindable[StreamRevision] {
    override def bind(key: String, params: Map[String, Seq[String]]) = params.get(key).flatMap(_.headOption).map { value =>
      try {
        Right(StreamRevision(value.toLong))
      } catch {
        case _: RuntimeException => Left("Cannot parse parameter " + key + " as StreamRevision: " + value)
      }
    }
    override def unbind(key: String, value: StreamRevision) = key + "=" + value.value
  }
}
