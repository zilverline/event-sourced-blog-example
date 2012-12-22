package events

import eventstore._
import java.util.UUID
import org.joda.time.DateTimeUtils
import org.scalacheck._, Arbitrary.arbitrary
import play.api.libs.json._
import IdentifierSpec._
import Generators._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PostEventsSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  import PostEventsSpec._

  "Post events" should {
    "convert to and from JSON" in eventsForMultiplePosts { events =>
      Json.fromJson[List[PostEvent]](Json.toJson(events)) must_== JsSuccess(events)
    }

    "parse example Post Added event" in {
      val event = PostAdded(PostId(UUID.fromString("5ab11526-477b-43b9-8fe6-4bb25a3dfcc6")), UserId(UUID.fromString("5ab11526-477b-43b9-8fe6-4bb25a3dfcc5")), PostContent(title = "Title", body = "Body"))
      val json = """{"type":"PostAdded","data":{"postId":"5ab11526-477b-43b9-8fe6-4bb25a3dfcc6","authorId":"5ab11526-477b-43b9-8fe6-4bb25a3dfcc5", "content":{"author":"Author","title":"Title","body":"Body"}}}"""

      Json.fromJson[PostEvent](Json.parse(json)) must_== JsSuccess(event)
    }
  }

  "Post comment event" should {
    val postId = PostId.generate
    val conflictsWith = implicitly[ConflictsWith[PostEvent]]
    val commentAdded = CommentAdded(postId, CommentId(1), CommentContent(Right("commenter"), "body"))
    val now = DateTimeUtils.currentTimeMillis

    def conflict(event: PostEvent) = Conflict(Seq(CommittedEvent(StoreRevision(1), now, postId.toString, StreamRevision(1), event)))

    "conflict with post comment with same id" in {
      conflictsWith.conflicting(
        conflict(commentAdded),
        Seq(commentAdded)) must beSome(conflict(commentAdded))
    }

    "not conflict with committed post comment event" in {
      conflictsWith.conflicting(
        conflict(commentAdded),
        Seq(PostDeleted(postId))) must beEmpty
    }

    "conflict with other post events" in {
      conflictsWith.conflicting(
        conflict(PostDeleted(postId)),
        Seq(PostEdited(postId, PostContent(title = "Title", body = "Body")))) must beSome(conflict(PostDeleted(postId)))
    }
  }
}

object PostEventsSpec {
  implicit val arbitraryPostContent: Arbitrary[PostContent] = Arbitrary(Gen.resultOf(PostContent.apply _))
  implicit val arbitraryCommentContent: Arbitrary[CommentContent] = Arbitrary(Gen.resultOf(CommentContent.apply _))

  implicit val shrinkPostEvents: Shrink[List[PostEvent]] = Shrink(_ => Stream.empty)

  def eventsForSinglePost(id: PostId): Arbitrary[List[PostEvent]] = Arbitrary(for {
    added <- Gen.resultOf(PostAdded(id, _: UserId, _: PostContent))
    edits <- Gen.resize(10, Gen.listOf(Gen.resultOf(PostEdited(id, _: PostContent))))
    commentCount <- Gen.chooseNum(0, 5)
    comments <- Gen.sequence[List, List[PostCommentEvent]](List.tabulate(commentCount) { i =>
      val commentId = CommentId(i + 1)
      for {
        added <- Gen.resultOf(CommentAdded(id, commentId, _: CommentContent))
        deleted <- Gen.frequency(3 -> Nil, 1 -> List(CommentDeleted(id, commentId)))
      } yield {
        added :: deleted
      }
    }).map(_.flatten)
    e <- interleaved(edits, comments)
    deleted <- Gen.frequency(3 -> Nil, 1 -> List(PostDeleted(id)))
  } yield added :: e ::: deleted)

  val eventsForMultiplePosts: Arbitrary[List[PostEvent]] = Arbitrary(for {
    events <- Gen.resize(10, Gen.listOf(arbitrary[PostId].flatMap { id => Gen.resize(5, arbitrary(eventsForSinglePost(id))) }))
  } yield events.flatten)
}
