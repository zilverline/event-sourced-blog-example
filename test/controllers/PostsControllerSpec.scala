package controllers

import play.api.test._
import play.api.test.Helpers._
import events._
import eventstore._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PostsControllerSpec extends support.Spec {
  val postId = PostId.generate()
  val postContent = PostContent(title = "title", body = "body")

  "posts controller" should {
    "list posts" in new fixture {
      given(PostAdded(postId, currentUserId, postContent): PostEvent)

      val response = subject.index(FakeRequest())

      status(response) must_== 200
      contentAsString(response) must contain("<td>Joe</td>")
    }

    "add post" in new fixture {
      val response = subject.add(postId)(authenticatedRequest.withFormUrlEncodedBody("title" -> "title", "body" -> "body"))

      status(response) must_== 303
      changes must_== Seq(PostAdded(postId, currentUserId, postContent))
    }

    "not allow adding post when not logged in" in new fixture {
      val response = subject.add(postId)(unauthenticatedRequest.withFormUrlEncodedBody("title" -> "title", "body" -> "body"))

      status(response) must_== 404
      changes must beEmpty
    }

    "edit post" in new fixture {
      given(PostAdded(postId, currentUserId, postContent): PostEvent)

      val response = subject.edit(postId, StreamRevision(1))(authenticatedRequest.withFormUrlEncodedBody("title" -> "edited title", "body" -> "edited body"))

      status(response) must_== 303
      changes must_== Seq(PostEdited(postId, PostContent(title = "edited title", body = "edited body")))
    }

    "not allow editing by unauthorized user" in new fixture {
      given(PostAdded(postId, currentUserId, postContent): PostEvent)

      val response = subject.edit(postId, StreamRevision(1))(unauthenticatedRequest.withFormUrlEncodedBody("title" -> "edited title", "body" -> "edited body"))

      status(response) must_== 404
      changes must beEmpty
    }

    "delete post" in new fixture {
      given(PostAdded(postId, currentUserId, postContent): PostEvent)

      val response = subject.delete(postId, StreamRevision(1))(authenticatedRequest)

      status(response) must_== 303
      header("Location", response) must beSome("/posts/")
      changes must_== Seq(PostDeleted(postId))
    }

    "not allow deleting post by unauthorized user" in new fixture {
      given(PostAdded(postId, currentUserId, postContent): PostEvent)

      val response = subject.delete(postId, StreamRevision(1))(unauthenticatedRequest)

      status(response) must_== 404
      changes must beEmpty
    }

    "add comment to post when logged in" in new fixture {
      given(PostAdded(postId, currentUserId, postContent): PostEvent)

      val response = subject.addComment(postId, StreamRevision(1))(authenticatedRequest.withFormUrlEncodedBody("body" -> "Body"))

      status(response) must_== 303
      changes must_== Seq(CommentAdded(postId, CommentId(1), CommentContent(Left(currentUserId), "Body")))
    }

    "add comment to post when not logged in" in new fixture {
      given(PostAdded(postId, currentUserId, postContent): PostEvent)

      val response = subject.addComment(postId, StreamRevision(1))(unauthenticatedRequest.withFormUrlEncodedBody("name" -> "Commenter", "body" -> "Body"))

      status(response) must_== 303
      changes must_== Seq(CommentAdded(postId, CommentId(1), CommentContent(Right("Commenter"), "Body")))
    }

    "delete comment by the post author" in new fixture {
      given(
        PostAdded(postId, currentUserId, postContent): PostEvent,
        CommentAdded(postId, CommentId(1), CommentContent(Right("Commenter"), "Body")): PostEvent)

      val response = subject.deleteComment(postId, StreamRevision(2), CommentId(1))(authenticatedRequest)

      status(response) must_== 303
      changes must_== Seq(CommentDeleted(postId, CommentId(1)))
    }

    "delete comments by the comment author" in new fixture {
      given(
        PostAdded(postId, UserId.generate(), postContent): PostEvent,
        CommentAdded(postId, CommentId(1), CommentContent(Left(currentUserId), "Body")): PostEvent)

      val response = subject.deleteComment(postId, StreamRevision(2), CommentId(1))(authenticatedRequest)

      status(response) must_== 303
      changes must_== Seq(CommentDeleted(postId, CommentId(1)))
    }

    "not allow deleting comments when not the post author or the comment author" in new fixture {
      given(
        PostAdded(postId, UserId.generate(), postContent): PostEvent,
        CommentAdded(postId, CommentId(1), CommentContent(Left(UserId.generate()), "Body")): PostEvent)

      val response = subject.deleteComment(postId, StreamRevision(2), CommentId(1))(authenticatedRequest)

      status(response) must_== 404
      changes must beEmpty
    }
  }

  trait fixture extends ControllerFixture {
    val subject = new PostsController(new MemoryImageActions(memoryImage).view(_.posts))
  }
}
