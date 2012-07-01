package controllers

import play.api.test._
import play.api.test.Helpers._
import events._
import models._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PostsControllerSpec extends org.specs2.mutable.Specification {
  isolated

  val postId = PostId.generate()
  val postContent = PostContent("author", "title", "content")
  val subject = new PostsController(Posts())

  "posts controller" should {
    "list posts" in {
      subject.commit(PostAdded(postId, postContent))

      val result = subject.index(FakeRequest())

      status(result) must_== 200
      contentAsString(result) must contain("<td>author</td>")
    }

    "add post" in {
      val result = subject.add.submit(postId)(FakeRequest().withFormUrlEncodedBody("author" -> "author", "title" -> "title", "content" -> "content"))

      status(result) must_== 303
      subject.posts() must_== Posts(Map(postId -> Post(postId, postContent)), Seq(postId))
    }

    "edit post" in {
      subject.commit(PostAdded(postId, postContent))

      val result = subject.edit.submit(postId)(FakeRequest().withFormUrlEncodedBody("author" -> "edited author", "title" -> "edited title", "content" -> "edited content"))

      status(result) must_== 303
      subject.posts() must_== Posts(Map(postId -> Post(postId, PostContent("edited author", "edited title", "edited content"))), Seq(postId))
    }

    "delete post" in {
      subject.commit(PostAdded(postId, postContent))

      val result = subject.delete(postId)(FakeRequest())

      status(result) must_== 303
      header("Location", result) must beSome("/posts/")
      subject.posts() must_== Posts()
    }
  }
}
