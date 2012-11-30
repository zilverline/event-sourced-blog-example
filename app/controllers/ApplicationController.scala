package controllers

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
    val state = memoryImage.get
    val applicationRequest = buildApplicationRequest(request, state)
    f(applicationRequest)
  }

  private def buildApplicationRequest[A](request: Request[A], state: models.State): ApplicationRequest[A] = {
    val authenticationToken = request.session.get("authenticationToken")
    val user = authenticationToken.flatMap(state.users.authenticated)
    val applicationRequest = ApplicationRequest(user, request)
    applicationRequest
  }
}
