package controllers

import scala.concurrent.stm._
import java.util.UUID
import play.api._
import play.api.Play.current
import play.api.mvc._
import events._
import models._
import eventstore.FakeEventStore

object Application extends Controller {

  val posts = Ref(Posts())
  val eventStore = new FakeEventStore[PostEvent](onCommit = { commit =>
    posts.single.transform { posts =>
      commit.events.map(_.payload).foldLeft(posts) { _ apply _ }
    }
  })

  if (Play.isDev) {
    var id = UUID.fromString("4e885ffe-870e-45b4-b5dd-f16d381d6f5f")
    eventStore.commit(id, 0, PostCreated(id, PostContent("Erik", "Scala is awesome", "Scala..."))) { _ => }
    id = UUID.fromString("4e885ffe-870e-45b4-b5dd-f16d381d6f6f")
    eventStore.commit(id, 0, PostCreated(id, PostContent("Bas", "Righteous Ruby", "Ruby..."))) { _ => }
  }

  def index = Action { implicit request =>
    Ok(views.html.index())
  }
}
