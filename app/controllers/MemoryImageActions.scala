package controllers

import events._
import eventstore._
import models._
import play.api.mvc._

/**
 * Implements `ApplicationActions` using the (global) memory image containing the
 * `ApplicationState`.
 */
class MemoryImageActions(memoryImage: MemoryImage[ApplicationState, DomainEvent])
    extends ApplicationActions[ApplicationState, DomainEvent] {

  def QueryAction(block: QueryBlock[AnyContent]): Action[AnyContent] = Action { implicit request =>
    val state = memoryImage.get
    block(state)(buildApplicationRequest(state))
  }

  def CommandAction(block: CommandBlock[AnyContent]) = Action { implicit request =>
    memoryImage.modify { state =>
      val applicationRequest = buildApplicationRequest(state)
      val currentUser = applicationRequest.currentUser
      val transaction = block(state)(applicationRequest)
      if (transaction.events.forall(currentUser.authorizeEvent(state))) {
        transaction.withHeaders(currentUser.registered.map(user => "currentUserId" -> user.id.toString).toSeq: _*)
      } else {
        Transaction.abort(notFound)
      }
    }
  }

  private def buildApplicationRequest[A](state: ApplicationState)(implicit request: Request[A]): ApplicationRequest[A] = {
    val authenticationToken = request.session.get("authenticationToken").flatMap(AuthenticationToken.fromString)
    val authenticatedUser = authenticationToken.flatMap(state.users.withAuthenticationToken)
    new ApplicationRequest(authenticatedUser getOrElse GuestUser, state.users, request)
  }
}
