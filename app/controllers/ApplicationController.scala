package controllers

import events._
import eventstore._
import models._
import play.api.mvc._

trait ApplicationRequestHeader extends RequestHeader {
  def currentUser: User
}
case class ApplicationRequest[A](currentUser: User, request: Request[A]) extends WrappedRequest(request) with ApplicationRequestHeader

trait ApplicationController[Event <: DomainEvent] extends Controller {
  def memoryImage: MemoryImage[ApplicationState, Event]

  /**
   * 404 Not Found response.
   */
  protected[this] def notFound(implicit request: Request[_]): Result = NotFound(views.html.defaultpages.notFound(request, None))

  type QueryBlock[A] = ApplicationState => ApplicationRequest[A] => Result
  type CommandBlock[A] = ApplicationState => ApplicationRequest[A] => Transaction[Event, Result]

  def QueryAction(block: QueryBlock[AnyContent]) = Action { implicit request =>
    val state = memoryImage.get
    block(state)(buildApplicationRequest(state))
  }

  def AuthenticatedQueryAction(block: RegisteredUser => QueryBlock[AnyContent]) = QueryAction {
    state => implicit request =>
      request.currentUser.registered map { user =>
        block(user)(state)(request)
      } getOrElse {
        notFound
      }
  }

  def CommandAction(block: CommandBlock[AnyContent]) = Action { implicit request =>
    memoryImage.modify { state =>
      val applicationRequest = buildApplicationRequest(state)
      val transaction = block(state)(applicationRequest)
      if (transaction.events.forall(applicationRequest.currentUser.authorizeEvent(state)))
        transaction
      else
        Transaction.abort(notFound)
    }
  }

  def AuthenticatedCommandAction(block: RegisteredUser => CommandBlock[AnyContent]) = CommandAction {
    state => implicit request =>
      request.currentUser.registered map { user =>
        block(user)(state)(request)
      } getOrElse {
        Transaction.abort(notFound)
      }
  }

  def ApplicationAction(block: ApplicationRequest[AnyContent] => Result) = QueryAction { _ => request => block(request) }

  private def buildApplicationRequest[A](state: models.ApplicationState)(implicit request: Request[A]): ApplicationRequest[A] = {
    val authenticationToken = request.session.get("authenticationToken").flatMap(AuthenticationToken.fromString)
    val authenticatedUser = authenticationToken.flatMap(state.users.authenticated)
    ApplicationRequest(authenticatedUser getOrElse Guest, request)
  }
}
