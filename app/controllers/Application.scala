package controllers

import scala.concurrent.stm._
import play.api._
import play.api.mvc._
import events._
import models._
import support.MemoryImage
import eventstore.FakeEventStore

object Application extends Controller {

  val posts = MemoryImage[Posts, PostEvent](Posts()) { _ apply _ }
  val eventStore = FakeEventStore[PostEvent] { commit =>
    atomic { implicit txn =>
      commit.events.foreach { event => posts.apply(event.payload) }
    }
  }

  def index = Action { implicit request =>
    Ok(views.html.index("Your new application is ready."))
  }

}
