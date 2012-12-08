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

  /**
   * 404 Not Found response.
   */
  protected[this] def notFound(implicit request: Request[_]): Result = NotFound(views.html.defaultpages.notFound(request, None))

  type QueryAction[A] = State => ApplicationRequest[A] => Result
  type CommandAction[A] = State => ApplicationRequest[AnyContent] => Transaction[Event, Result]

  def QueryAction(block: QueryAction[AnyContent]) = Action { request =>
    val state = memoryImage.get
    block(state)(buildApplicationRequest(request, state))
  }

  def AuthenticatedQueryAction(block: User => QueryAction[AnyContent]) = QueryAction {
    state => implicit request =>
      request.currentUser map { user =>
        block(user)(state)(request)
      } getOrElse {
        notFound
      }
  }

  def CommandAction(block: CommandAction[AnyContent]) = Action { request =>
    memoryImage.modify { state =>
      block(state)(buildApplicationRequest(request, state))
    }
  }

  def AuthenticatedCommandAction(block: User => CommandAction[AnyContent]) = CommandAction {
    state => implicit request =>
      request.currentUser map { user =>
        block(user)(state)(request)
      } getOrElse {
        Transaction.abort(notFound)
      }
  }

  def ApplicationAction(block: ApplicationRequest[AnyContent] => Result) = QueryAction { _ => request => block(request) }

  private def buildApplicationRequest[A](request: Request[A], state: models.State): ApplicationRequest[A] = {
    val authenticationToken = request.session.get("authenticationToken").flatMap(AuthenticationToken.fromString)
    val user = authenticationToken.flatMap(state.users.authenticated)
    ApplicationRequest(user, request)
  }
}
