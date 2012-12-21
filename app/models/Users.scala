package models

import events._
import eventstore._

trait CurrentUserContext {
  /**
   * The current authenticated user or the guest user.
   */
  def currentUser: User
}
trait UsersContext {
  def users: Users
}

sealed trait User {
  def displayName: String

  def registered: Option[RegisteredUser] = None

  def canAddPost: Boolean = false
  def canEditPost(post: Post): Boolean = false
  def canDeletePost(post: Post): Boolean = false
  def canDeleteComment(post: Post, comment: Comment): Boolean = false

  def authorizeEvent(state: ApplicationState): DomainEvent => Boolean = _ => false
}

case object GuestUser extends User {
  def displayName = "Guest"

  override def authorizeEvent(state: ApplicationState): DomainEvent => Boolean = {
    case _: UserRegistered => true
    case _: UserLoggedIn   => true
    case _: CommentAdded   => true
    case _                 => false
  }
}

case class PseudonymousUser(displayName: String) extends User

case class UnknownUser(id: UserId) extends User {
  def displayName = "[unknown]"
}

case class RegisteredUser(id: UserId, revision: StreamRevision, emailAddress: EmailAddress, displayName: String, password: Password, authenticationToken: Option[AuthenticationToken] = None) extends User {
  override def registered = Some(this)

  override def canAddPost = true
  override def canEditPost(post: Post) = post.isAuthoredBy(this)
  override def canDeletePost(post: Post) = post.isAuthoredBy(this)
  override def canDeleteComment(post: Post, comment: Comment) = post.isAuthoredBy(this) || comment.isAuthoredBy(this)

  override def authorizeEvent(state: ApplicationState): DomainEvent => Boolean = {
    case event: UserRegistered      => false
    case event: UserPasswordChanged => id == event.userId
    case event: UserLoggedIn        => true
    case event: UserLoggedOut       => id == event.userId
    case event: PostAdded           => id == event.authorId
    case event: PostEdited          => state.posts.get(event.postId).exists(this canEditPost _)
    case event: PostDeleted         => state.posts.get(event.postId).exists(this canDeletePost _)
    case event: CommentAdded        => true
    case event: CommentDeleted =>
      (for {
        post <- state.posts.get(event.postId)
        comment <- post.comments.get(event.commentId)
      } yield {
        this.canDeleteComment(post, comment)
      }) getOrElse {
        false
      }
    case _ => false
  }
}

case class Users(
    private val byId: Map[UserId, RegisteredUser] = Map.empty,
    private val byEmail: Map[EmailAddress, UserId] = Map.empty,
    private val byAuthenticationToken: Map[AuthenticationToken, UserId] = Map.empty) {

  def get(id: UserId): Option[RegisteredUser] = byId.get(id)

  def withEmail(email: EmailAddress): Option[RegisteredUser] =
    byEmail.get(email).flatMap(get)

  def withAuthenticationToken(token: AuthenticationToken): Option[RegisteredUser] =
    byAuthenticationToken.get(token).flatMap(get)

  def authenticate(email: EmailAddress, password: String): Option[(RegisteredUser, AuthenticationToken)] =
    withEmail(email).filter(_.password.verify(password)).map { (_, AuthenticationToken.generate) }

  def update(event: UserEvent, revision: StreamRevision): Users = event match {
    case UserRegistered(userId, email, displayName, password) =>
      val user = RegisteredUser(userId, revision, email, displayName, password)
      copy(byId = byId.updated(userId, user), byEmail = byEmail.updated(email, userId))

    case UserPasswordChanged(userId, password) =>
      val user = byId(userId)
      copy(byId = byId.updated(userId, user.copy(revision = revision, password = password)))

    case UserLoggedIn(userId, authenticationToken) =>
      val user = byId(userId)
      copy(
        byId = byId.updated(userId, byId(userId).copy(revision = revision, authenticationToken = Some(authenticationToken))),
        byAuthenticationToken = user.authenticationToken.foldLeft(byAuthenticationToken) { _ - _ }.updated(authenticationToken, userId))

    case UserLoggedOut(userId) =>
      val user = byId(userId)
      copy(
        byId = byId.updated(userId, user.copy(revision = revision, authenticationToken = None)),
        byAuthenticationToken = user.authenticationToken.foldLeft(byAuthenticationToken) { _ - _ })
  }
}
