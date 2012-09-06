package models

import org.scalacheck._, Arbitrary.arbitrary, Prop.{ forAll, forAllNoShrink }
import events._, PostEventsSpec._
import eventstore._
import eventstore.fake.FakeEventStore
import scala.collection.immutable.SortedMap

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PostsSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  "posts" should {
    val A = PostId.generate()
    val B = PostId.generate()
    val Content = PostContent(author = "author", title = "title", body = "body")
    val Updated = Content.copy(body = "updated")

    "contain post with content from last non-delete event" in {
      given event PostAdded(A, Content) thenPostWithId A must beSome(Post(A, StreamRevision(1), Content))
      given events (PostAdded(A, Content), PostEdited(A, Updated)) thenPostWithId A must beSome(Post(A, StreamRevision(2), Updated))
      given events (PostAdded(A, Content), PostDeleted(A)) thenPostWithId A must beNone

      forAllNoShrink(arbitrary(eventsForSinglePost(A))) { events =>
        val posts = given.events(events: _*).posts
        events.filterNot(_.isInstanceOf[PostCommentEvent]).last match {
          case PostAdded(id, content)  => posts.get(id).map(_.content) must beSome(content)
          case PostEdited(id, content) => posts.get(id).map(_.content) must beSome(content)
          case PostDeleted(id)         => posts.get(id) must beNone
          case _                       => failure("unknown event")
        }
      }
    }

    "contain all non-deleted posts in reverse order of adding" in {
      given event PostAdded(A, Content) thenMostRecent 1 must_== Seq(Post(A, StreamRevision(1), Content))
      given events (PostAdded(A, Content), PostAdded(B, Updated)) thenMostRecent 2 must_== Seq(Post(B, StreamRevision(1), Updated), Post(A, StreamRevision(1), Content))
      given events (PostAdded(A, Content), PostEdited(A, Updated)) thenMostRecent 1 must_== Seq(Post(A, StreamRevision(2), Updated))
      given events (PostAdded(A, Content), PostDeleted(A)) thenMostRecent 1 must beEmpty

      forAllNoShrink(arbitrary(eventsForMultiplePosts)) { events =>
        val posts = given.events(events: _*).posts
        val deletedIds = events.collect { case PostDeleted(id) => id }.toSet
        val remainingIds = events.collect { case PostAdded(id, _) => id }.filterNot(deletedIds)

        posts.mostRecent(Int.MaxValue) must_== remainingIds.flatMap(posts.get).reverse
      }
    }

    val CommentContent_1 = CommentContent("Commenter", "Body 1")
    "track comments and the next comment id" in {
      given events (PostAdded(A, Content), CommentAdded(A, CommentId(1), CommentContent_1)) thenPostWithId A must beSome(Post(A, StreamRevision(2), Content, CommentId(2), SortedMap(CommentId(1) -> CommentContent_1)))
      given events (PostAdded(A, Content), CommentAdded(A, CommentId(1), CommentContent_1), CommentDeleted(A, CommentId(1))) thenPostWithId A must beSome(Post(A, StreamRevision(3), Content, CommentId(2), SortedMap.empty))

      forAllNoShrink(arbitrary(eventsForSinglePost(A))) { events =>
        !events.last.isInstanceOf[PostDeleted] ==> {
          val post = given.events(events: _*).posts.get(A).getOrElse(failure("post not found"))
          val commentsAdded = events.collect { case e: CommentAdded => e }
          val commentsDeleted = events.collect { case e: CommentDeleted => e }.map(_.commentId).toSet
          val remaining = commentsAdded.filterNot(e => commentsDeleted.contains(e.commentId)).map(e => (e.commentId, e.content)).toMap

          post.nextCommentId must_== CommentId(commentsAdded.size + 1)
          remaining must_== post.comments
        }
      }
    }
  }

  object given {
    def event(event: PostEvent) = events(event)
    def events(events: PostEvent*) = new {
      val posts = {
        val eventStore = FakeEventStore.fromHistory(events)
        try {
          val commits = eventStore.reader.readCommits(StoreRevision.Initial, StoreRevision.Maximum)
          commits.flatMap(_.eventsWithRevision).foldLeft(Posts())((posts, event) => posts.update(event._1, event._2))
        } finally {
          eventStore.close
        }
      }
      def thenPostWithId(id: PostId) = posts.get(id)
      def thenMostRecent(n: Int) = posts.mostRecent(n)
    }
  }
}
