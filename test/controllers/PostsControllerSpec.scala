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
  val author = UserId.generate()
  val postContent = PostContent(title = "title", body = "body")

  "posts controller" should {
    "list posts" in new fixture {
      eventStore.committer.tryCommit(Changes(StreamRevision.Initial, PostAdded(postId, author, postContent): PostEvent)) must beRight

      val result = subject.index(FakeRequest())

      status(result) must_== 200

      contentAsString(result) must contain("<td>" + author + "</td>")
    }

    "add post" in new fixture {
      val result = subject.add.submit(postId)(authenticatedRequest.withFormUrlEncodedBody("title" -> "title", "body" -> "body"))

      status(result) must_== 303
      posts.get(postId) must beSome(Post(postId, StreamRevision.Initial.next, currentUserId, postContent))
    }

    "edit post" in new fixture {
      eventStore.committer.tryCommit(Changes(StreamRevision.Initial, PostAdded(postId, author, postContent): PostEvent))

      val result = subject.edit.submit(postId, StreamRevision(1))(FakeRequest().withFormUrlEncodedBody("title" -> "edited title", "body" -> "edited body"))

      status(result) must_== 303
      posts.get(postId) must beSome(Post(postId, StreamRevision(2), author, PostContent(title = "edited title", body = "edited body")))
    }

    "delete post" in new fixture {
      eventStore.committer.tryCommit(Changes(StreamRevision.Initial, PostAdded(postId, author, postContent): PostEvent))

      val result = subject.delete(postId, StreamRevision(1))(FakeRequest())

      status(result) must_== 303
      header("Location", result) must beSome("/posts/")
      posts.byId must beEmpty
      posts.orderedByTimeAdded must beEmpty
    }

    "add comment to post" in new fixture {
      eventStore.committer.tryCommit(Changes(StreamRevision.Initial, PostAdded(postId, author, postContent): PostEvent))

      val result = subject.comments.add(postId, StreamRevision(1))(FakeRequest().withFormUrlEncodedBody("commenter" -> "Commenter", "body" -> "Body"))

      status(result) must_== 303
      posts.get(postId).map(_.comments) must beSome(SortedMap(CommentId(1) -> CommentContent("Commenter", "Body")))
    }

    "delete comment from post" in new fixture {
      eventStore.committer.tryCommit(Changes(StreamRevision(0), PostAdded(postId, author, postContent): PostEvent))
      eventStore.committer.tryCommit(Changes(StreamRevision(1), CommentAdded(postId, CommentId(1), CommentContent("Commenter", "Body")): PostEvent))

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

    val memoryImage = MemoryImage[State, DomainEvent](eventStore)(State()) {
      (state, commit) => state.updateMany(commit.eventsWithRevision)
    }

    val subject = new PostsController(memoryImage)

    val authenticatedRequest = FakeRequest().withSession("authenticationToken" -> authenticationToken.toString)

    def posts = memoryImage.get.posts

    override def after {
      eventStore.close
      Play.stop()
    }
  }
}
