package controllers

import play.api.test._
import play.api.test.Helpers._
import events._
import eventstore._
import models._
import org.specs2.mutable.After
import scala.collection.immutable.SortedMap
import play.api.Play

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PostsControllerSpec extends org.specs2.mutable.Specification {
  val postId = PostId.generate()
  val postContent = PostContent(title = "title", body = "body")

  "posts controller" should {
    "list posts" in new fixture {
      given(PostAdded(postId, currentUserId, postContent): PostEvent)

      val result = subject.index(FakeRequest())

      status(result) must_== 200
      contentAsString(result) must contain("<td>Joe</td>")
    }

    "add post" in new fixture {
      val result = subject.add.submit(postId)(authenticatedRequest.withFormUrlEncodedBody("title" -> "title", "body" -> "body"))

      status(result) must_== 303
      changes must_== Seq(PostAdded(postId, currentUserId, postContent))
    }

    "not allow adding post when not logged in" in new fixture {
      val result = subject.add.submit(postId)(unauthenticatedRequest.withFormUrlEncodedBody("title" -> "title", "body" -> "body"))

      status(result) must_== 401
      changes must beEmpty
    }

    "edit post" in new fixture {
      given(PostAdded(postId, currentUserId, postContent): PostEvent)

      val result = subject.edit.submit(postId, StreamRevision(1))(authenticatedRequest.withFormUrlEncodedBody("title" -> "edited title", "body" -> "edited body"))

      status(result) must_== 303
      changes must_== Seq(PostEdited(postId, PostContent(title = "edited title", body = "edited body")))
    }

    "not allow editing by unauthorized user" in new fixture {
      given(PostAdded(postId, currentUserId, postContent): PostEvent)

      val result = subject.edit.submit(postId, StreamRevision(1))(FakeRequest().withFormUrlEncodedBody("title" -> "edited title", "body" -> "edited body"))

      status(result) must_== 404
      changes must beEmpty
    }

    "delete post" in new fixture {
      given(PostAdded(postId, currentUserId, postContent): PostEvent)

      val result = subject.delete(postId, StreamRevision(1))(authenticatedRequest)

      status(result) must_== 303
      header("Location", result) must beSome("/posts/")
      changes must_== Seq(PostDeleted(postId))
    }

    "not allow deleting post by unauthorized user" in new fixture {
      given(PostAdded(postId, currentUserId, postContent): PostEvent)

      val result = subject.delete(postId, StreamRevision(1))(unauthenticatedRequest)

      status(result) must_== 404
      changes must beEmpty
    }

    "add comment to post when logged in" in new fixture {
      given(PostAdded(postId, currentUserId, postContent): PostEvent)

      val result = subject.comments.add(postId, StreamRevision(1))(authenticatedRequest.withFormUrlEncodedBody("body" -> "Body"))

      status(result) must_== 303
      changes must_== Seq(CommentAdded(postId, CommentId(1), CommentContent(Left(currentUserId), "Body")))
    }

    "add comment to post when not logged in" in new fixture {
      given(PostAdded(postId, currentUserId, postContent): PostEvent)

      val result = subject.comments.add(postId, StreamRevision(1))(unauthenticatedRequest.withFormUrlEncodedBody("name" -> "Commenter", "body" -> "Body"))

      status(result) must_== 303
      changes must_== Seq(CommentAdded(postId, CommentId(1), CommentContent(Right("Commenter"), "Body")))
    }

    "delete comment from post" in new fixture {
      given(
        PostAdded(postId, currentUserId, postContent): PostEvent,
        CommentAdded(postId, CommentId(1), CommentContent(Left(currentUserId), "Body")): PostEvent)

      val result = subject.comments.delete(postId, StreamRevision(2), CommentId(1))(unauthenticatedRequest)

      status(result) must_== 303
      changes must_== Seq(CommentDeleted(postId, CommentId(1)))
    }
  }

  trait fixture extends After { self =>
    Play.start(FakeApplication())

    val currentUserId = UserId.generate()
    val authenticationToken = AuthenticationToken.generate()
    val unauthenticatedRequest = FakeRequest()
    val authenticatedRequest = FakeRequest().withSession("authenticationToken" -> authenticationToken.toString)

    val eventStore: EventStore[DomainEvent] = new fake.FakeEventStore

    var initialStoreRevision = eventStore.reader.storeRevision
    def given[StreamId, Event <: DomainEvent](events: Event*)(implicit eventStreamType: EventStreamType[StreamId, Event]) {
      for (event <- events) {
        val revision = eventStore.reader.streamRevision(eventStreamType.streamId(event))
        eventStore.committer.tryCommit(Changes(revision, event)) must beRight
      }
      initialStoreRevision = eventStore.reader.storeRevision
    }

    val memoryImage = MemoryImage[State, DomainEvent](eventStore)(State()) {
      (state, commit) => state.updateMany(commit.eventsWithRevision)
    }

    def changes = eventStore.reader.readCommits(initialStoreRevision, StoreRevision.Maximum).flatMap(_.events).toSeq

    given(
      UserRegistered(currentUserId, EmailAddress("joe@example.com"), "Joe", Password.fromPlainText("password")): UserEvent,
      UserLoggedIn(currentUserId, authenticationToken): UserEvent)

    val subject = new PostsController(memoryImage)

    override def after {
      eventStore.close
      Play.stop()
    }
  }
}
