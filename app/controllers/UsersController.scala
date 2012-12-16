package controllers

import events._
import eventstore._
import eventstore.Transaction._
import models._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import support.Forms._

object UsersController extends UsersController(Global.MemoryImageActions.view(_.users), Global.emailAddressRegistry)
class UsersController(actions: ApplicationActions[Users, UserEvent], registerEmailAddress: EmailAddress => UserId) {
  import actions._

  private def unexpectedConflict(conflict: Conflict[UserEvent]) = InternalServerError

  object authentication {
    val loginForm = Form(tuple("email" -> emailAddress, "password" -> password))

    def show = ApplicationAction { implicit request => Ok(views.html.users.logIn(loginForm)) }

    def submit = CommandAction { users => implicit request =>
      val form = loginForm.bindFromRequest
      form.fold(
        formWithErrors => abort(BadRequest(views.html.users.logIn(formWithErrors))),
        credentials => users.authenticate(credentials._1, credentials._2) map {
          case (user, token) =>
            Changes(user.revision, UserLoggedIn(user.id, token): UserEvent).commit(
              onCommit = Redirect(routes.UsersController.authentication.loggedIn).withSession("authenticationToken" -> token.toString),
              onConflict = unexpectedConflict)
        } getOrElse {
          abort(BadRequest(views.html.users.logIn(form.addError(FormError("", "bad.credentials")))))
        })
    }

    def logOut = CommandAction { state => implicit request =>
      def loggedOut = Redirect(routes.UsersController.authentication.loggedOut).withNewSession
      request.currentUser.registered.map { user =>
        Changes(user.revision, UserLoggedOut(user.id): UserEvent).commit(
          onCommit = loggedOut,
          onConflict = unexpectedConflict)
      } getOrElse {
        abort(loggedOut)
      }
    }

    def loggedIn = AuthenticatedQueryAction { _ => _ => implicit request => Ok(views.html.users.loggedIn()) }
    def loggedOut = ApplicationAction { implicit request => Ok(views.html.users.loggedOut()) }
  }

  object register {
    val confirmPasswordMapping: Mapping[Password] = tuple(
      "1" -> password.verifying(minLength(8)),
      "2" -> password).
      verifying("error.password.mismatch", fields => fields._1 == fields._2).
      transform(fields => Password.fromPlainText(fields._1), _ => ("", ""))

    /**
     * Registration form.
     */
    val registrationForm: Form[(EmailAddress, String, Password)] = Form(tuple(
      "email" -> emailAddress,
      "displayName" -> tokenizedText.verifying(minLength(2)),
      "password" -> confirmPasswordMapping))

    def show = ApplicationAction { implicit request =>
      Ok(views.html.users.register(registrationForm))
    }
    def submit = CommandAction { state => implicit request =>
      val form = registrationForm.bindFromRequest
      form.fold(
        formWithErrors =>
          abort(BadRequest(views.html.users.register(formWithErrors))),
        registration => {
          val (email, displayName, password) = registration
          val userId = registerEmailAddress(email)
          Changes(StreamRevision.Initial, UserRegistered(userId, email, displayName, password): UserEvent).commit(
            onCommit = Redirect(routes.UsersController.register.registered),
            onConflict = _ => BadRequest(views.html.users.register(form.addError(FormError("", "duplicate.account")))))
        })
    }
    def registered = ApplicationAction { implicit request =>
      Ok(views.html.users.registered())
    }
  }
}
