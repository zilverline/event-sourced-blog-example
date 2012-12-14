package controllers

import events._
import eventstore._
import models._
import play.api.mvc._

class MemoryImageActions[State, Event](
  memoryImage: MemoryImage[State, Event],
  findRegisteredUser: (AuthenticationToken, State) => Option[RegisteredUser],
  authorizeChange: (User, State) => Event => Boolean)
    extends ApplicationActions[State, Event] {

  def QueryAction(block: QueryBlock[AnyContent]): Action[AnyContent] = Action { implicit request =>
    val state = memoryImage.get
    block(state)(buildApplicationRequest(state))
  }

  def CommandAction(block: CommandBlock[AnyContent]) = Action { implicit request =>
    memoryImage.modify { state =>
      val applicationRequest = buildApplicationRequest(state)
      val transaction = block(state)(applicationRequest)
      if (transaction.events.forall(authorizeChange(applicationRequest.currentUser, state)))
        transaction
      else
        Transaction.abort(notFound)
    }
  }

  private def buildApplicationRequest[A](state: State)(implicit request: Request[A]): ApplicationRequest[A] = {
    val authenticationToken = request.session.get("authenticationToken").flatMap(AuthenticationToken.fromString)
    val authenticatedUser = authenticationToken.flatMap(token => findRegisteredUser(token, state))
    ApplicationRequest(authenticatedUser getOrElse Guest, request)
  }
}
