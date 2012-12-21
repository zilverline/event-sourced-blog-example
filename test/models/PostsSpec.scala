package models

import events._, PostEventsSpec._
import eventstore._
import scala.collection.immutable.SortedMap

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PostsSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  "posts" should {
    val A = PostId.generate()
    val B = PostId.generate()
    val AuthorId = UserId.generate()
    val AuthorDisplayName = "Joe"
    val Content = PostContent(title = "title", body = "body")
    val Updated = Content.copy(body = "updated")

    "contain added post" in {
      val post = given(PostAdded(A, AuthorId, Content)).post(A)
      post must_== Post(A, StreamRevision(1), AuthorId, Content)
    }

    "contain edited post" in {
      val post = given(PostAdded(A, AuthorId, Content), PostEdited(A, Updated)).post(A)
      post must_== Post(A, StreamRevision(2), AuthorId, Updated)
    }

    "remove deleted post" in {
      val posts = given(PostAdded(A, AuthorId, Content), PostDeleted(A)).posts
      posts.get(A) must beNone
    }

    "track the stream revision per post" in eventsForMultiplePosts { events =>
      val fixture = given(events: _*)
      fixture.streamRevisions must haveAllElementsLike {
        case (postId, revision) => fixture.posts.get(postId).map(_.revision must_== revision).getOrElse(ok)
      }
    }

    "contain all non-deleted posts in reverse order of adding" in eventsForSinglePost(A) { events =>
      val posts = given(events: _*).posts

      events.filterNot(_.isInstanceOf[PostCommentEvent]).last match {
        case PostAdded(id, _, content) => posts.get(id).map(_.content) must beSome(content)
        case PostEdited(id, content)   => posts.get(id).map(_.content) must beSome(content)
        case PostDeleted(id)           => posts.get(id) must beNone
        case unknown                   => failure("unknown event: " + unknown)
      }
    }

    val CommentContent_1 = CommentContent(Right("Commenter"), "Body 1")
    "contain added comment" in {
      val post = given(PostAdded(A, AuthorId, Content), CommentAdded(A, CommentId(1), CommentContent_1)).post(A)

      post.nextCommentId must_== CommentId(2)
      post.comments must_== SortedMap(CommentId(1) -> Comment(CommentId(1), CommentContent_1))
    }

    "remove deleted comment" in {
      val post = given(
        PostAdded(A, AuthorId, Content),
        CommentAdded(A, CommentId(1), CommentContent_1), CommentDeleted(A, CommentId(1))).post(A)

      post.nextCommentId must_== CommentId(2)
      post.comments must beEmpty
    }

    "track comments per post" in eventsForSinglePost(A) { events =>
      !events.last.isInstanceOf[PostDeleted] ==> {
        val post = given(events: _*).post(A)

        val commentsAdded = events.collect { case e: CommentAdded => e }
        val commentsDeleted = events.collect { case e: CommentDeleted => e }.map(_.commentId).toSet
        val remaining = commentsAdded.filterNot(e => commentsDeleted.contains(e.commentId)).map(e => (e.commentId, Comment(e.commentId, e.content))).toMap

        post.nextCommentId must_== CommentId(commentsAdded.size + 1)
        remaining must_== post.comments
      }
    }
  }

  case class given(events: PostEvent*) extends eventstore.MemoryImageFixture(events: _*) {
    val posts = eventsWithRevision.foldLeft(Posts()) {
      case (posts, (event, revision)) => posts.update(event, revision)
    }

    def post(id: PostId) = posts.get(id).getOrElse(failure("post not found: " + id))
  }
}
