package controllers

import events._
import eventstore._
import eventstore.Transaction._
import models._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import scala.annotation.tailrec
import support.Mappings._
import play.api.http.HeaderNames

object UsersController extends UsersController(Global.persistence.memoryImage)
class UsersController(memoryImage: MemoryImage[State, UserEvent]) extends Controller {
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

    def show = Action { implicit request =>
      Ok(views.html.users.register(registrationForm))
    }
    def submit = Action { implicit request =>
      registrationForm.bindFromRequest.fold(
        formWithErrors => BadRequest(views.html.users.register(formWithErrors)),
        registration =>
          memoryImage.modify { _ =>
            val userId = UserId.generate()
            Changes(StreamRevision.Initial, UserRegistered(userId, registration._1, registration._2): UserEvent).commit(
              onCommit = Redirect(routes.UsersController.register.registered(userId)),
              onConflict = conflict => sys.error("conflict"))
          })
    }
    def registered(id: UserId) = Action { implicit request =>
      users.byId.get(id) map { user =>
        Ok(views.html.users.registered(user))
      } getOrElse {
        NotFound(views.html.defaultpages.notFound(request, None))
      }
    }
  }

  /**
   * The registered users.
   */
  private[this] def users(): Users = memoryImage.get.users

  /**
   * 404 Not Found response.
   */
  private[this] def notFound(implicit request: Request[_]): Result = NotFound(views.html.defaultpages.notFound(request, None))

  /**
   * Runs the transaction `body` against the post identified by `postId` and
   * returns the result, if it exists. Otherwise `None` is returned.
   */
  private[this] def updatePost[A](id: PostId)(body: Post => Transaction[UserEvent, A]): Option[A] =
    memoryImage.modify { _.posts.get(id).map(body).sequence }
}
