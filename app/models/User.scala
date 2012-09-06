package models

import events._
import eventstore._

case class User(userId: UserId, revision: StreamRevision, login: Email, password: Password)

case class Users(byId: Map[UserId, User] = Map.empty, byLogin: Map[Email, UserId] = Map.empty) {
  def get(login: Email) = byLogin.get(login).map(byId)

  def update(event: UserEvent, revision: StreamRevision): Users = event match {
    case UserRegistered(userId, login, password) =>
      val user = User(userId, revision, login, password)
      copy(byId = byId.updated(userId, user), byLogin = byLogin.updated(login, userId))
    case UserPasswordChanged(userId, password) =>
      copy(byId = byId.updated(userId, byId(userId).copy(password = password)))
  }
}
