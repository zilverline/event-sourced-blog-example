package controllers

import events._
import eventstore._
import eventstore.Transaction._
import models._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.i18n.Messages
import support.Forms._

object UsersController extends UsersController(Global.MemoryImageActions.view(_.users), Global.emailAddressRegistry.claim)
class UsersController(actions: ApplicationActions[Users, UserEvent], registerEmailAddress: (EmailAddress, UserId) => UserId) {
  import actions._

  private def unexpectedConflict(conflict: Conflict[UserEvent]) = InternalServerError

  private val confirmedPassword: Mapping[Password] =
    tuple("1" -> password.verifying(minLength(8)), "2" -> password)
      .verifying("error.password.mismatch", fields => fields._1 == fields._2)
      .transform(fields => Password.fromPlainText(fields._1), _ => ("", ""))

  private val loginForm = Form(tuple("email" -> emailAddress, "password" -> password))

  def showLogIn = ApplicationAction { implicit request => Ok(views.html.users.logIn(loginForm)) }

  def logIn = CommandAction { users => implicit request =>
    val form = loginForm.bindFromRequest
    form.fold(
      formWithErrors => abort(BadRequest(views.html.users.logIn(formWithErrors))),
      credentials => users.authenticate(credentials._1, credentials._2) map {
        case (user, token) =>
          Changes(user.revision, UserLoggedIn(user.id, token): UserEvent).commit(
            onCommit = Redirect(routes.UsersController.loggedIn).withSession("authenticationToken" -> token.toString),
            onConflict = unexpectedConflict)
      } getOrElse {
        abort(BadRequest(views.html.users.logIn(form.addError(FormError("", "bad.credentials")))))
      })
  }

  def logOut = CommandAction { state => implicit request =>
    def loggedOut = Redirect(routes.UsersController.loggedOut).withNewSession
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

  /**
   * Registration form.
   */
  private val registrationForm: Form[(EmailAddress, String, Password)] = Form(tuple(
    "email" -> emailAddress,
    "displayName" -> tokenizedText.verifying(minLength(2)),
    "password" -> confirmedPassword))

  def showRegister = ApplicationAction { implicit request =>
    Ok(views.html.users.register(registrationForm))
  }

  def register = CommandAction { state => implicit request =>
    val form = registrationForm.bindFromRequest
    form.fold(
      formWithErrors =>
        abort(BadRequest(views.html.users.register(formWithErrors))),
      registration => {
        val (email, displayName, password) = registration
        val userId = registerEmailAddress(email, UserId.generate)
        Changes(StreamRevision.Initial, UserRegistered(userId, email, displayName, password): UserEvent).commit(
          onCommit = Redirect(routes.UsersController.registered),
          onConflict = _ => BadRequest(views.html.users.register(form.addError(FormError("", "duplicate.account")))))
      })
  }

  def registered = ApplicationAction { implicit request =>
    Ok(views.html.users.registered())
  }

  private def changeProfileForm(implicit user: RegisteredUser) =
    Form(single("displayName" -> tokenizedText.verifying(minLength(2)))).fill(user.displayName)

  private def changeEmailForm(implicit user: RegisteredUser) =
    Form(single("email" -> emailAddress.verifying("value.unchanged", _ != user.email))
      .verifying("duplicate.account", email => registerEmailAddress(email, user.id) == user.id))
      .fill(user.email)

  private def changePasswordForm(implicit user: RegisteredUser) =
    Form(tuple(
      "password.old" -> text.verifying("current.password.incorrect", user.password.verify _),
      "password.new" -> confirmedPassword))

  def showProfile = AuthenticatedQueryAction { implicit user => _ => implicit request =>
    Ok(views.html.users.profile(changeProfileForm, changeEmailForm, changePasswordForm))
  }

  def changeProfile = AuthenticatedCommandAction { implicit user => _ => implicit request =>
    changeProfileForm.bindFromRequest.fold(
      formWithErrors =>
        abort(BadRequest(views.html.users.profile(formWithErrors, changeEmailForm, changePasswordForm))),
      profile =>
        Changes(user.revision, UserProfileChanged(user.id, profile): UserEvent).commit(
          onCommit = Redirect(routes.UsersController.showProfile).flashing("info" -> Messages("profile.updated")),
          onConflict = unexpectedConflict))
  }

  def changeEmailAddress = AuthenticatedCommandAction { implicit user => _ => implicit request =>
    changeEmailForm.bindFromRequest.fold(
      formWithErrors =>
        abort(BadRequest(views.html.users.profile(changeProfileForm, formWithErrors, changePasswordForm))),
      email =>
        Changes(user.revision, UserEmailAddressChanged(user.id, email): UserEvent).commit(
          onCommit = Redirect(routes.UsersController.showProfile).flashing("info" -> Messages("email.updated")),
          onConflict = unexpectedConflict))
  }

  def changePassword = AuthenticatedCommandAction { implicit user => _ => implicit request =>
    changePasswordForm.bindFromRequest.fold(
      formWithErrors =>
        abort(BadRequest(views.html.users.profile(changeProfileForm, changeEmailForm, formWithErrors))),
      password =>
        Changes(user.revision, UserPasswordChanged(user.id, password._2): UserEvent).commit(
          onCommit = Redirect(routes.UsersController.showProfile).flashing("info" -> Messages("password.updated")),
          onConflict = unexpectedConflict))
  }
}
