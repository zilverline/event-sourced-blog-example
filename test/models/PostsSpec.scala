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
      given event PostCreated(A, Content) thenPostWithId A must beSome(Post(A, Content))
      given events (PostCreated(A, Content), PostUpdated(A, Updated)) thenPostWithId A must beSome(Post(A, Updated))
      given events (PostCreated(A, Content), PostDeleted(A)) thenPostWithId A must beNone

      forAllNoShrink(arbitrary(eventsForSinglePost(A))) { events =>
        val posts = Posts(events)
        events.last match {
          case PostCreated(id, content) => posts.get(id) must beSome(Post(id, content))
          case PostUpdated(id, content) => posts.get(id) must beSome(Post(id, content))
          case PostDeleted(id)          => posts.get(id) must beNone
        }
      }
    }

    "contain all non-deleted posts in reverse order of creation" in {
      given event PostCreated(A, Content) thenMostRecent 1 must_== Seq(Post(A, Content))
      given events (PostCreated(A, Content), PostCreated(B, Updated)) thenMostRecent 2 must_== Seq(Post(B, Updated), Post(A, Content))
      given events (PostCreated(A, Content), PostUpdated(A, Updated)) thenMostRecent 1 must_== Seq(Post(A, Updated))
      given events (PostCreated(A, Content), PostDeleted(A)) thenMostRecent 1 must beEmpty

      forAllNoShrink(arbitrary(eventsForMultiplePosts)) { events =>
        val posts = Posts(events)
        val deletedIds = events.collect { case PostDeleted(id) => id }.toSet
        val expectedIds = events.collect { case PostCreated(id, _) => id }.filterNot(deletedIds).reverse

        posts.mostRecent(Int.MaxValue) must_== expectedIds.map(posts)
      }
    }
  }

  object given {
    def event(event: PostEvent) = events(event)
    def events(events: PostEvent*) = new {
      def thenPostWithId(id: PostId) = Posts(events).get(id)
      def thenMostRecent(n: Int) = Posts(events).mostRecent(n)
    }
  }
}
