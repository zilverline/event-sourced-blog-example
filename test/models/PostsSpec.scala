package models

import org.scalacheck._, Arbitrary.arbitrary, Prop.{ forAll, forAllNoShrink }
import events._, PostEventsSpec._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PostsSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  "posts" should {
    val A = PostId.generate()
    val B = PostId.generate()
    val Content = PostContent("author", "title", "content")
    val Updated = Content.copy(content = "updated")

    "contain post with content from last non-delete event" in {
      given event PostAdded(A, Content) thenPostWithId A must beSome(Post(A, Content))
      given events (PostAdded(A, Content), PostEdited(A, Updated)) thenPostWithId A must beSome(Post(A, Updated))
      given events (PostAdded(A, Content), PostDeleted(A)) thenPostWithId A must beNone

      forAllNoShrink(arbitrary(eventsForSinglePost(A))) { events =>
        val posts = Posts.fromHistory(events: _*)
        events.last match {
          case PostAdded(id, content)  => posts.get(id) must beSome(Post(id, content))
          case PostEdited(id, content) => posts.get(id) must beSome(Post(id, content))
          case PostDeleted(id)         => posts.get(id) must beNone
        }
      }
    }

    "contain all non-deleted posts in reverse order of adding" in {
      given event PostAdded(A, Content) thenMostRecent 1 must_== Seq(Post(A, Content))
      given events (PostAdded(A, Content), PostAdded(B, Updated)) thenMostRecent 2 must_== Seq(Post(B, Updated), Post(A, Content))
      given events (PostAdded(A, Content), PostEdited(A, Updated)) thenMostRecent 1 must_== Seq(Post(A, Updated))
      given events (PostAdded(A, Content), PostDeleted(A)) thenMostRecent 1 must beEmpty

      forAllNoShrink(arbitrary(eventsForMultiplePosts)) { events =>
        val posts = Posts.fromHistory(events: _*)
        val deletedIds = events.collect { case PostDeleted(id) => id }.toSet
        val remainingIds = events.collect { case PostAdded(id, _) => id }.filterNot(deletedIds)

        posts.mostRecent(Int.MaxValue) must_== remainingIds.flatMap(posts.get).reverse
      }
    }
  }

  object given {
    def event(event: PostEvent) = events(event)
    def events(events: PostEvent*) = new {
      val posts = Posts.fromHistory(events: _*)
      def thenPostWithId(id: PostId) = posts.get(id)
      def thenMostRecent(n: Int) = posts.mostRecent(n)
    }
  }
}
