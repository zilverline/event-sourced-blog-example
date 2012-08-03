package support

import java.net.URLDecoder
import java.util.UUID
import events.CommentId
import eventstore.{ StoreRevision, StreamRevision }

object Binders {
  implicit def IdentifierPathBindable[A <: Identifier](implicit companion: IdentifierCompanion[A]) = new play.api.mvc.PathBindable[A] {
    override def bind(key: String, value: String) = try {
      Right(companion.apply(UUID.fromString(URLDecoder.decode(value, "utf-8"))))
    } catch {
      case _: RuntimeException => Left("Cannot parse parameter " + key + " as " + companion.prefix + ": " + URLDecoder.decode(value, "utf-8"))
    }
    override def unbind(key: String, value: A) = value.uuid.toString
  }

  implicit object CommentIdBindable extends play.api.mvc.PathBindable[CommentId] {
    override def bind(key: String, value: String) = try {
      Right(CommentId(URLDecoder.decode(value, "utf-8").toInt))
    } catch {
      case _: RuntimeException => Left("Cannot parse parameter " + key + " as CommentId: " + URLDecoder.decode(value, "utf-8"))
    }
    override def unbind(key: String, value: CommentId) = value.value.toString
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

  implicit object queryStringBindableStreamRevision extends play.api.mvc.QueryStringBindable[StreamRevision] {
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
