package controllers

import events.AuthenticationToken
import eventstore._
import models._
import play.api.mvc._

trait ApplicationRequestHeader extends RequestHeader {
  def currentUser: Option[User]
}
case class ApplicationRequest[A](currentUser: Option[User], request: Request[A]) extends WrappedRequest(request) with ApplicationRequestHeader

trait ApplicationController[Event] extends Controller {
  def memoryImage: MemoryImage[State, Event]

  def QueryAction(f: State => ApplicationRequest[AnyContent] => Result) = Action { request =>
    val state = memoryImage.get
    f(state)(buildApplicationRequest(request, state))
  }

  def CommandAction(f: State => ApplicationRequest[AnyContent] => Transaction[Event, Result]) = Action { request =>
    memoryImage.modify { state =>
      f(state)(buildApplicationRequest(request, state))
    }
  }

  def ApplicationAction(f: ApplicationRequest[AnyContent] => Result) = QueryAction { _ => request => f(request) }

  private def buildApplicationRequest[A](request: Request[A], state: models.State): ApplicationRequest[A] = {
    val authenticationToken = request.session.get("authenticationToken").flatMap(AuthenticationToken.fromString)
    val user = authenticationToken.flatMap(state.users.authenticated)
    ApplicationRequest(user, request)
  }
}
