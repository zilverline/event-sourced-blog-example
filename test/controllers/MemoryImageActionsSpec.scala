package controllers

import events._
import eventstore._, Transaction._
import play.api.mvc.Results._
import play.api.test.Helpers._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class MemoryImageActionsSpec extends org.specs2.mutable.Specification {
  "Memory image actions" should {
    "commit changes to event store" in new fixture {
      val response = testCommandAction.apply(authenticatedRequest)

      status(response) must_== 200
      changes must_== Seq(postAdded)
    }

    "forbid unauthorized changes" in new fixture {
      val response = testCommandAction.apply(unauthenticatedRequest)

      status(response) must_== 404
      changes must beEmpty
    }

    "add current user id to commit headers" in new fixture {
      testCommandAction.apply(authenticatedRequest)

      val commit = commits.headOption.getOrElse(failure("no commit"))
      commit.headers must contain("currentUserId" -> currentUserId.toString)
    }
  }

  trait fixture extends ControllerFixture {
    val postId = PostId.generate
    val postAdded: PostEvent = PostAdded(postId, currentUserId, PostContent("title", "body"))
    val subject = new MemoryImageActions(memoryImage)
    val testCommandAction = subject.CommandAction { state => implicit request =>
      Changes(StreamRevision.Initial, postAdded).commit(onCommit = Ok, onConflict = _ => failure("conflict"))
    }
  }
}
