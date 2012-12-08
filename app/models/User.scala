package models

import events._
import eventstore._

case class User(userId: UserId, revision: StreamRevision, emailAddress: EmailAddress, displayName: String, password: Password, authenticationToken: Option[AuthenticationToken] = None)

case class Users(byId: Map[UserId, User] = Map.empty, byLogin: Map[EmailAddress, UserId] = Map.empty, byAuthenticationToken: Map[AuthenticationToken, UserId] = Map.empty) {
  def get(login: EmailAddress) = byLogin.get(login).map(byId)

  def authenticated(authenticationToken: AuthenticationToken): Option[User] = byAuthenticationToken.get(authenticationToken).map(byId)

  def update(event: UserEvent, revision: StreamRevision): Users = event match {
    case UserRegistered(userId, login, displayName, password) =>
      val user = User(userId, revision, login, displayName, password)
      copy(byId = byId.updated(userId, user), byLogin = byLogin.updated(login, userId))
    case UserPasswordChanged(userId, password) =>
      copy(byId = byId.updated(userId, byId(userId).copy(revision = revision, password = password)))
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
