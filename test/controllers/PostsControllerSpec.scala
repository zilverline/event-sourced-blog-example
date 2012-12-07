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
      given(PostAdded(postId, currentUserId, postContent))

      val result = subject.index(FakeRequest())

      status(result) must_== 200
      contentAsString(result) must contain("<td>Joe</td>")
    }

    "add post" in new fixture {
      val result = subject.add.submit(postId)(authenticatedRequest.withFormUrlEncodedBody("title" -> "title", "body" -> "body"))

      status(result) must_== 303
      posts.get(postId) must beSome(Post(postId, StreamRevision.Initial.next, currentUserId, "Joe", postContent))
    }

    "not allow adding post when not logged in" in new fixture {
      val result = subject.add.submit(postId)(FakeRequest().withFormUrlEncodedBody("title" -> "title", "body" -> "body"))

      status(result) must_== 401
      changes must beEmpty
    }

    "edit post" in new fixture {
      given(PostAdded(postId, currentUserId, postContent))

      val result = subject.edit.submit(postId, StreamRevision(1))(authenticatedRequest.withFormUrlEncodedBody("title" -> "edited title", "body" -> "edited body"))

      status(result) must_== 303
      posts.get(postId) must beSome(Post(postId, StreamRevision(2), currentUserId, "Joe", PostContent(title = "edited title", body = "edited body")))
    }

    "not allow editing by unauthorized user" in new fixture {
      given(PostAdded(postId, currentUserId, postContent))

      val result = subject.edit.submit(postId, StreamRevision(1))(FakeRequest().withFormUrlEncodedBody("title" -> "edited title", "body" -> "edited body"))

      status(result) must_== 404
      changes must beEmpty
    }

    "delete post" in new fixture {
      given(PostAdded(postId, currentUserId, postContent))

      val result = subject.delete(postId, StreamRevision(1))(authenticatedRequest)

      status(result) must_== 303
      header("Location", result) must beSome("/posts/")
      posts.byId must beEmpty
      posts.orderedByTimeAdded must beEmpty
    }

    "not allow deleting post by unauthorized user" in new fixture {
      given(PostAdded(postId, currentUserId, postContent))

      val result = subject.delete(postId, StreamRevision(1))(FakeRequest())

      status(result) must_== 404
      changes must beEmpty
    }

    "add comment to post" in new fixture {
      given(PostAdded(postId, currentUserId, postContent))

      val result = subject.comments.add(postId, StreamRevision(1))(FakeRequest().withFormUrlEncodedBody("commenter" -> "Commenter", "body" -> "Body"))

      status(result) must_== 303
      posts.get(postId).map(_.comments) must beSome(SortedMap(CommentId(1) -> CommentContent("Commenter", "Body")))
    }

    "delete comment from post" in new fixture {
      given(PostAdded(postId, currentUserId, postContent), CommentAdded(postId, CommentId(1), CommentContent("Commenter", "Body")))

      val result = subject.comments.delete(postId, StreamRevision(2), CommentId(1))(FakeRequest())

      status(result) must_== 303
      posts.get(postId).map(_.comments.get(CommentId(1))) must beSome(None)
    }
  }

  trait fixture extends After { self =>
    Play.start(FakeApplication())

    val currentUserId = UserId.generate()
    val authenticationToken = AuthenticationToken.generate()

    val eventStore: EventStore[DomainEvent] = new fake.FakeEventStore

    eventStore.committer.tryCommit(Changes(currentUserId, StreamRevision.Initial,
      UserRegistered(currentUserId, EmailAddress("joe@example.com"), "Joe", Password.fromPlainText("password")): UserEvent,
      UserLoggedIn(currentUserId, authenticationToken): UserEvent))
    var initialStoreRevision = eventStore.reader.storeRevision

    def given(events: PostEvent*)(implicit eventStreamType: EventStreamType[PostId, PostEvent]) {
      for (event <- events) {
        val revision = eventStore.reader.streamRevision(event.postId)
        eventStore.committer.tryCommit(Changes(revision, event)) must beRight
      }
      initialStoreRevision = eventStore.reader.storeRevision
    }

    val memoryImage = MemoryImage[State, DomainEvent](eventStore)(State()) {
      (state, commit) => state.updateMany(commit.eventsWithRevision)
    }

    val subject = new PostsController(memoryImage)

    val authenticatedRequest = FakeRequest().withSession("authenticationToken" -> authenticationToken.toString)

    def posts = memoryImage.get.posts

    def changes = eventStore.reader.readCommits(initialStoreRevision, StoreRevision.Maximum)

    override def after {
      eventStore.close
      Play.stop()
    }
  }
}
