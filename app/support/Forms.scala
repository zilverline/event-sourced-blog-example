package support

import play.api.data._
import play.api.data.Forms._

object Forms {
  val trimmedText = text.transform[String](_.trim, identity)
  val tokenizedText = text.transform[String](_.trim.replaceAll("\\s+", " "), identity)

  implicit def FormOps[T](form: Form[T]) = new {
    def addError(error: FormError) = form.copy(errors = form.errors :+ error)
  }
}
