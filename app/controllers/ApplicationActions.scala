package controllers

import events._
import eventstore._
import models._
import play.api.mvc._
import play.api.mvc.Results._

trait ApplicationRequestHeader extends RequestHeader {
  def currentUser: User
}
case class ApplicationRequest[A](
    currentUser: User,
    request: Request[A]) extends WrappedRequest(request) with ApplicationRequestHeader

/**
 * Actions available to a controller that makes use of a memory image.
 */
trait ApplicationActions[State, -Event] extends Controller { outer =>
  /**
   * The type of blocks that only need to query the current state of the memory image.
   */
  type QueryBlock[A] = State => ApplicationRequest[A] => Result

  /**
   * The type of blocks that wish to read and modify the memory image.
   */
  type CommandBlock[A] = State => ApplicationRequest[A] => Transaction[Event, Result]

  /**
   * Runs the given `block` using the current state of the memory image.
   */
  def QueryAction(block: QueryBlock[AnyContent]): Action[AnyContent]

  /**
   * Runs the given `block` using the current state of the memory image and applies the resulting
   * transaction.
   */
  def CommandAction(block: CommandBlock[AnyContent]): Action[AnyContent]

  /**
   * Convenience method for actions that need to know the current user but do not need access to
   * the current state of the memory image.
   */
  def ApplicationAction(block: ApplicationRequest[AnyContent] => Result): Action[AnyContent] =
    QueryAction { _ => request => block(request) }

  /**
   * Runs the given query `block` with the current authenticated user. Returns `notFound` if there
   * is no authenticated user.
   */
  def AuthenticatedQueryAction(block: RegisteredUser => QueryBlock[AnyContent]): Action[AnyContent] =
    QueryAction { state => implicit request =>
      request.currentUser.registered map { user =>
        block(user)(state)(request)
      } getOrElse {
        notFound
      }
    }

  /**
   * Runs the given command `block` with the current authenticated user. Returns `notFound` if there
   * is no authenticated user.
   */
  def AuthenticatedCommandAction(block: RegisteredUser => CommandBlock[AnyContent]): Action[AnyContent] =
    CommandAction { state => implicit request =>
      request.currentUser.registered map { user =>
        block(user)(state)(request)
      } getOrElse {
        Transaction.abort(notFound)
      }
    }

  def view[S, E <: Event](f: State => S): ApplicationActions[S, E] = new ApplicationActions[S, E] {
    def QueryAction(block: QueryBlock[AnyContent]) = outer.QueryAction { state => request =>
      block(f(state))(request)
    }
    def CommandAction(block: CommandBlock[AnyContent]) = outer.CommandAction { state => request =>
      block(f(state))(request)
    }
  }

  /**
   * 404 Not Found response.
   */
  def notFound(implicit request: Request[_]): Result = NotFound(views.html.defaultpages.notFound(request, None))
}
