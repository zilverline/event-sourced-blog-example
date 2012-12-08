package controllers

import events._
import eventstore._
import models._
import play.api.mvc._

trait ApplicationRequestHeader extends RequestHeader {
  def currentUser: Option[User]
}
case class ApplicationRequest[A](currentUser: Option[User], request: Request[A]) extends WrappedRequest(request) with ApplicationRequestHeader

trait ApplicationController[Event <: DomainEvent] extends Controller {
  def memoryImage: MemoryImage[State, Event]

  /**
   * 404 Not Found response.
   */
  protected[this] def notFound(implicit request: Request[_]): Result = NotFound(views.html.defaultpages.notFound(request, None))

  type QueryAction[A] = State => ApplicationRequest[A] => Result
  type CommandAction[A] = State => ApplicationRequest[AnyContent] => Transaction[Event, Result]

  def QueryAction(block: QueryAction[AnyContent]) = Action { implicit request =>
    val state = memoryImage.get
    block(state)(buildApplicationRequest(state))
  }

  def AuthenticatedQueryAction(block: User => QueryAction[AnyContent]) = QueryAction {
    state => implicit request =>
      request.currentUser map { user =>
        block(user)(state)(request)
      } getOrElse {
        notFound
      }
  }

  def CommandAction(block: CommandAction[AnyContent]) = Action { implicit request =>
    memoryImage.modify { state =>
      val applicationRequest = buildApplicationRequest(state)
      val transaction = block(state)(applicationRequest)
      if (Authorization.authorizeChanges(applicationRequest.currentUser, state, transaction.events))
        transaction
      else
        Transaction.abort(notFound)
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

  private def buildApplicationRequest[A](state: models.State)(implicit request: Request[A]): ApplicationRequest[A] = {
    val authenticationToken = request.session.get("authenticationToken").flatMap(AuthenticationToken.fromString)
    val user = authenticationToken.flatMap(state.users.authenticated)
    ApplicationRequest(user, request)
  }
}
