package controllers

import play.api.test._
import play.api.test.Helpers._
import events._
import eventstore._
import models._
import org.specs2.mutable.After

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PostsControllerSpec extends org.specs2.mutable.Specification {
  val postId = PostId.generate()
  val postContent = PostContent(author = "author", title = "title", body = "body")

  "posts controller" should {
    "list posts" in new fixture {
      memoryImage.tryCommit(postId.toString, StreamRevision.Initial, PostAdded(postId, postContent)) must beRight

      val result = subject.index(FakeRequest())

      status(result) must_== 200

      contentAsString(result) must contain("<td>author</td>")
    }

    "add post" in new fixture {
      val result = subject.add.submit(postId)(FakeRequest().withFormUrlEncodedBody("author" -> "author", "title" -> "title", "body" -> "body"))

      status(result) must_== 303
      subject.posts().get(postId) must beSome(Post(postId, StreamRevision.Initial.next, postContent))
    }

    "edit post" in new fixture {
      memoryImage.tryCommit(postId.toString, StreamRevision.Initial, PostAdded(postId, postContent))

      val result = subject.edit.submit(postId, StreamRevision(1))(FakeRequest().withFormUrlEncodedBody("author" -> "edited author", "title" -> "edited title", "body" -> "edited body"))

      status(result) must_== 303
      subject.posts().get(postId) must beSome(Post(postId, StreamRevision(2), PostContent(author = "edited author", title = "edited title", body = "edited body")))
    }

    "delete post" in new fixture {
      memoryImage.tryCommit(postId.toString, StreamRevision.Initial, PostAdded(postId, postContent))

      val result = subject.delete(postId, StreamRevision(1))(FakeRequest())

      status(result) must_== 303
      header("Location", result) must beSome("/posts/")
      subject.posts().byId must beEmpty
      subject.posts().orderedByTimeAdded must beEmpty
    }
  }

  trait fixture extends After { self =>
    val eventStore: EventStore[PostEvent] = new fake.FakeEventStore

    val memoryImage = MemoryImage[Posts, PostEvent](eventStore)(Posts()) {
      (posts, commit) => posts.updateMany(commit.eventsWithRevision)
    }

    val subject = new PostsController(memoryImage)

    override def after {
      eventStore.close
    }
  }
}
