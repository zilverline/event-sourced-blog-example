package support

import play.api.data.Forms._

object Mappings {
  val trimmedText = text.transform[String](_.trim, identity)
}
