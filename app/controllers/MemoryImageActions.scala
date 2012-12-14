package controllers

import events._
import eventstore._
import models._
import play.api.mvc._

class MemoryImageActions(memoryImage: MemoryImage[ApplicationState, DomainEvent])
    extends ApplicationActions[ApplicationState, DomainEvent] {

  def QueryAction(block: QueryBlock[AnyContent]): Action[AnyContent] = Action { implicit request =>
    val state = memoryImage.get
    block(state)(buildApplicationRequest(state))
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

  private def buildApplicationRequest[A](state: ApplicationState)(implicit request: Request[A]): ApplicationRequest[A] = {
    val authenticationToken = request.session.get("authenticationToken").flatMap(AuthenticationToken.fromString)
    val authenticatedUser = authenticationToken.flatMap(token => state.users.authenticated(token))
    new ApplicationRequest(authenticatedUser getOrElse Guest, state.users, request)
  }
}
