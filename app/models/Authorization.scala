package models

import events._

object Authorization {
  def authorizeChanges(currentUser: Option[User], state: State, events: Seq[DomainEvent]): Boolean =
    events.forall(currentUser.map(authorizeAuthenticated).getOrElse(authorizeAnonymous(_))(state))

  def authorizeAuthenticated(user: User)(state: State): DomainEvent => Boolean = {
    case event: UserRegistered      => false
    case event: UserPasswordChanged => event.userId == user.id
    case event: UserLoggedIn        => true
    case event: UserLoggedOut       => event.userId == user.id
    case event: PostAdded           => event.authorId == user.id
    case event: PostEdited          => state.posts.get(event.postId).exists(_ isAuthoredBy user)
    case event: PostDeleted         => state.posts.get(event.postId).exists(_ isAuthoredBy user)
    case event: CommentAdded        => true
    case event: CommentDeleted =>
      (for {
        post <- state.posts.get(event.postId)
        comment <- post.comments.get(event.commentId)
        if post.isAuthoredBy(user) || comment.isAuthoredBy(user)
      } yield ()).isDefined
    case _ => false
  }

  def authorizeAnonymous(state: State): DomainEvent => Boolean = {
    case _: UserRegistered => true
    case _: UserLoggedIn   => true
    case _: CommentAdded   => true
    case _                 => false
  }
}
