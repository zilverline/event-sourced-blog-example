package controllers

import events._
import eventstore._
import models._
import play.api.mvc._

/**
 * Implements `ControllerActions` using the (global) memory image containing the
 * `ApplicationState`.
 */
class MemoryImageActions(memoryImage: MemoryImage[ApplicationState, DomainEvent])
    extends ControllerActions[ApplicationState, DomainEvent] {

  override def QueryAction(block: QueryBlock[AnyContent]) = Action { request =>
    val state = memoryImage.get
    block(state)(buildApplicationRequest(request, state))
  }

  override def CommandAction(block: CommandBlock[AnyContent]) = Action { request =>
    memoryImage.modify { state =>
      val applicationRequest = buildApplicationRequest(request, state)
      val currentUser = applicationRequest.currentUser
      val transaction = block(state)(applicationRequest)
      if (transaction.events.forall(currentUser.authorizeEvent(state))) {
        transaction.withHeaders(currentUser.registered.map(user => "currentUserId" -> user.id.toString).toSeq: _*)
      } else {
        Transaction.abort(notFound(request))
      }
    }
  }

  private def buildApplicationRequest[A](request: Request[A], state: ApplicationState) = {
    val currentUser = request.session.get("authenticationToken")
      .flatMap(AuthenticationToken.fromString)
      .flatMap(state.users.findByAuthenticationToken)
      .getOrElse(GuestUser)
    new ApplicationRequest(request, currentUser, state.users)
  }
}
