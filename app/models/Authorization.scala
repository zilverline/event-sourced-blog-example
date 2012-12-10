package models

import events._

object Authorization {
  def authorizeChanges(currentUser: User, state: ApplicationState, events: Seq[DomainEvent]): Boolean = {
    val authorized = currentUser.registered.map(authorizeUser _).getOrElse(authorizeGuest _)(state)
    events.forall(authorized)
  }

  private def authorizeUser(user: RegisteredUser)(state: ApplicationState): DomainEvent => Boolean = {
    case event: UserRegistered      => false
    case event: UserPasswordChanged => event.userId == user.id
    case event: UserLoggedIn        => true
    case event: UserLoggedOut       => event.userId == user.id
    case event: PostAdded           => event.authorId == user.id
    case event: PostEdited          => state.posts.get(event.postId).exists(user canEditPost _)
    case event: PostDeleted         => state.posts.get(event.postId).exists(user canDeletePost _)
    case event: CommentAdded        => true
    case event: CommentDeleted =>
      (for {
        post <- state.posts.get(event.postId)
        comment <- post.comments.get(event.commentId)
      } yield {
        user.canDeleteComment(post, comment)
      }) getOrElse {
        false
      }
    case _ => false
  }

  private def authorizeGuest(state: ApplicationState): DomainEvent => Boolean = {
    case _: UserRegistered => true
    case _: UserLoggedIn   => true
    case _: CommentAdded   => true
    case _                 => false
  }
}
