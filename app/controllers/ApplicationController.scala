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

  def ApplicationAction(f: ApplicationRequest[AnyContent] => Result) = Action { request =>
    f(buildApplicationRequest(request, memoryImage.get))
  }

  private def buildApplicationRequest[A](request: Request[A], state: models.State): ApplicationRequest[A] = {
    val authenticationToken = request.session.get("authenticationToken").flatMap(AuthenticationToken.fromString)
    val user = authenticationToken.flatMap(state.users.authenticated)
    ApplicationRequest(user, request)
  }
}
