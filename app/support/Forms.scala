package support

import play.api.data.Forms._

object Forms {
  val trimmedText = text.transform[String](_.trim, identity)
}
