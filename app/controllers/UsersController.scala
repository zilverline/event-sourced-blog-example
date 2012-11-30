package controllers

import events._
import eventstore._
import eventstore.Transaction._
import java.security.SecureRandom
import models._
import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.http.HeaderNames
import play.api.mvc._
import scala.annotation.tailrec
import support.Mappings._

object UsersController extends UsersController(Global.persistence.memoryImage)
class UsersController(override val memoryImage: MemoryImage[State, UserEvent]) extends ApplicationController[UserEvent] {
  object authentication {
    private[this] val random = new SecureRandom()
    def generateAuthenticationToken() = "%08x-%08x".format(random.nextLong, random.nextLong)
    def authenticate(users: Users)(login: Email, password: String): Option[(User, String)] =
      users.get(login).filter(_.password.verify(password)).map { user => (user, generateAuthenticationToken()) }

    val loginForm: Form[(Email, String)] = Form(tuple(
      "email" -> email.transform[Email](Email.apply, _.value),
      "password" -> text.transform[String](identity, _ => "")))

    def show = ApplicationAction { implicit request => Ok(views.html.users.log_in(loginForm)) }

    def submit = ApplicationAction { implicit request =>
      loginForm.bindFromRequest.fold(
        formWithErrors => BadRequest(views.html.users.log_in(formWithErrors)),
        authentication => memoryImage.modify { state =>
          authenticate(state.users)(authentication._1, authentication._2) map {
            case (user, token) =>
              Changes(user.revision, UserLoggedIn(user.userId, token): UserEvent).commit(
                onCommit = Redirect(routes.UsersController.authentication.loggedIn).withSession("authenticationToken" -> token),
                onConflict = conflict => sys.error("conflict"))
          } getOrElse {
            abort(BadRequest(views.html.users.log_in(loginForm.copy(errors = Seq(FormError("", "Your email and password did not match a known account."))))))
          }
        })
    }

    def logOut = ApplicationAction { implicit request =>
      request.currentUser.map { user =>
        memoryImage.modify { state =>
          Changes(user.revision, UserLoggedOut(user.userId): UserEvent).commit(
            onCommit = Redirect(routes.UsersController.authentication.loggedOut).withNewSession,
            onConflict = conflict => sys.error("conflict"))
        }
      } getOrElse {
        Redirect(routes.UsersController.authentication.loggedOut).withNewSession
      }
    }

    def loggedIn = ApplicationAction { implicit request => Ok(views.html.users.logged_in()) }
    def loggedOut = ApplicationAction { implicit request => Ok(views.html.users.logged_out()) }
  }

  object register {
    val confirmPasswordMapping: Mapping[Password] = tuple(
      "1" -> text.verifying(minLength(8)),
      "2" -> text).
      verifying("error.password.mismatch", fields => fields._1 == fields._2).
      transform(fields => Password.fromPlainText(fields._1), _ => ("", ""))

    /**
     * Registration form.
     */
    val registrationForm: Form[(Email, Password)] = Form(tuple(
      "email" -> email.transform[Email](Email.apply, _.value),
      "password" -> confirmPasswordMapping))

    def show = ApplicationAction { implicit request =>
      Ok(views.html.users.register(registrationForm))
    }
    def submit = ApplicationAction { implicit request =>
      registrationForm.bindFromRequest.fold(
        formWithErrors =>
          BadRequest(views.html.users.register(formWithErrors)),
        registration => memoryImage.modify { state =>
          val userId = UserId.generate()
          Changes(StreamRevision.Initial, UserRegistered(userId, registration._1, registration._2): UserEvent).commit(
            onCommit = Redirect(routes.UsersController.register.registered),
            onConflict = conflict => sys.error("conflict"))
        })
    }
    def registered = ApplicationAction { implicit request =>
        request.currentUser map { user =>
          Ok(views.html.users.registered(user))
        } getOrElse {
          NotFound(views.html.defaultpages.notFound(request, None))
        }
    }
  }

  /**
   * 404 Not Found response.
   */
  private[this] def notFound(implicit request: Request[_]): Result = NotFound(views.html.defaultpages.notFound(request, None))
}
