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
    val Author = UserId.generate()
    val AuthorDisplayName = "Joe"
    val Content = PostContent(title = "title", body = "body")
    val Updated = Content.copy(body = "updated")

    "contain post with content from last non-delete event" in {
      given event PostAdded(A, Author, Content) thenPostWithId A must beSome(Post(A, StreamRevision(1), Author, AuthorDisplayName, Content))
      given events (PostAdded(A, Author, Content), PostEdited(A, Updated)) thenPostWithId A must beSome(Post(A, StreamRevision(2), Author, AuthorDisplayName, Updated))
      given events (PostAdded(A, Author, Content), PostDeleted(A)) thenPostWithId A must beNone

      forAllNoShrink(arbitrary(eventsForSinglePost(A))) { events =>
        val posts = given.events(events: _*).posts
        events.filterNot(_.isInstanceOf[PostCommentEvent]).last match {
          case PostAdded(id, _, content)  => posts.get(id).map(_.content) must beSome(content)
          case PostEdited(id, content) => posts.get(id).map(_.content) must beSome(content)
          case PostDeleted(id)         => posts.get(id) must beNone
          case _                       => failure("unknown event")
        }
      }
    }

    "contain all non-deleted posts in reverse order of adding" in {
      given event PostAdded(A, Author, Content) thenMostRecent 1 must_== Seq(Post(A, StreamRevision(1), Author, AuthorDisplayName, Content))
      given events (PostAdded(A, Author, Content), PostAdded(B, Author, Updated)) thenMostRecent 2 must_== Seq(Post(B, StreamRevision(1), Author, AuthorDisplayName, Updated), Post(A, StreamRevision(1), Author, AuthorDisplayName, Content))
      given events (PostAdded(A, Author, Content), PostEdited(A, Updated)) thenMostRecent 1 must_== Seq(Post(A, StreamRevision(2), Author, AuthorDisplayName, Updated))
      given events (PostAdded(A, Author, Content), PostDeleted(A)) thenMostRecent 1 must beEmpty

      forAllNoShrink(arbitrary(eventsForMultiplePosts)) { events =>
        val posts = given.events(events: _*).posts
        val deletedIds = events.collect { case PostDeleted(id) => id }.toSet
        val remainingIds = events.collect { case PostAdded(id, _, _) => id }.filterNot(deletedIds)

        posts.mostRecent(Int.MaxValue) must_== remainingIds.flatMap(posts.get).reverse
      }
    }

    val CommentContent_1 = CommentContent(Right("Commenter"), "Body 1")
    "track comments and the next comment id" in {
      given events (PostAdded(A, Author, Content), CommentAdded(A, CommentId(1), CommentContent_1)) thenPostWithId A must beSome(Post(A, StreamRevision(2), Author, AuthorDisplayName, Content, CommentId(2), SortedMap(CommentId(1) -> Comment(CommentId(1), "Commenter", CommentContent_1))))
      given events (PostAdded(A, Author, Content), CommentAdded(A, CommentId(1), CommentContent_1), CommentDeleted(A, CommentId(1))) thenPostWithId A must beSome(Post(A, StreamRevision(3), Author, AuthorDisplayName, Content, CommentId(2), SortedMap.empty))

      forAllNoShrink(arbitrary(eventsForSinglePost(A))) { events =>
        !events.last.isInstanceOf[PostDeleted] ==> {
          val post = given.events(events: _*).posts.get(A).getOrElse(failure("post not found"))
          val commentsAdded = events.collect { case e: CommentAdded => e }
          val commentsDeleted = events.collect { case e: CommentDeleted => e }.map(_.commentId).toSet
          val remaining = commentsAdded.filterNot(e => commentsDeleted.contains(e.commentId)).map(e => (e.commentId, Comment(e.commentId, e.content.commenter.fold(_ => "Joe", identity), e.content))).toMap

          post.nextCommentId must_== CommentId(commentsAdded.size + 1)
          remaining must_== post.comments
        }
      }
    }
  }

  object given {
    val password = Password.fromPlainText("password")

    def event(event: PostEvent) = events(event)
    def events(events: PostEvent*) = new {
      val posts = {
        val eventStore = FakeEventStore.fromHistory(events)
        try {
          val commits = eventStore.reader.readCommits(StoreRevision.Initial, StoreRevision.Maximum)
          commits.flatMap(_.eventsWithRevision).foldLeft(Posts())((posts, event) => posts.update(event._1, event._2, userId => Some(RegisteredUser(userId, StreamRevision(1), EmailAddress("joe@example.com"), "Joe", password))))
        } finally {
          eventStore.close
        }
      }
      def thenPostWithId(id: PostId) = posts.get(id)
      def thenMostRecent(n: Int) = posts.mostRecent(n)
    }
  }
}
