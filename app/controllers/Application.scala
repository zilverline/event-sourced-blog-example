package controllers

object Application {
  import Global.MemoryImageActions._

  def index = ApplicationAction { implicit request =>
    Ok(views.html.index())
  }
}
