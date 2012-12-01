package events

import eventstore._
import java.util.UUID
import org.joda.time.DateTimeUtils
import org.scalacheck._, Arbitrary.arbitrary, Prop.forAll
import play.api.libs.json._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PostEventsSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  import PostEventsSpec._

  "PostId" should {
    "be equal to itself" in forAll { (id: PostId) =>
      id must_== id
    }

    "generate unique values" in forAll { (a: PostId, b: PostId) =>
      a must_!= b
    }

    "convert to and from Strings" in forAll { (id: PostId) =>
      PostId.fromString(id.toString) must beSome(id)
    }

    "convert to and from JSON" in forAll { (id: PostId) =>
      Json.fromJson[PostId](Json.toJson(id)) must_== id
    }

    "fail to parse invalid strings" in forAll { (s: String) =>
      PostId.fromString(s) match {
        case Some(postId) => postId.toString must_== s
        case None         => ok
      }
    }
  }

  "Post events" should {
    "convert to and from JSON" in forAll(eventsForMultiplePosts.arbitrary) { events =>
      Json.fromJson[List[PostEvent]](Json.toJson(events)) must_== events
    }

    "parse example Post Added event" in {
      val event = PostAdded(PostId(UUID.fromString("5ab11526-477b-43b9-8fe6-4bb25a3dfcc6")), PostContent(author = "Author", title = "Title", body = "Body"))
      val json = """{"type":"PostAdded","data":{"postId":"5ab11526-477b-43b9-8fe6-4bb25a3dfcc6","content":{"author":"Author","title":"Title","body":"Body"}}}"""

      Json.fromJson[PostEvent](Json.parse(json)) must_== event
    }
  }

  "Post comment event" should {
    val postId = PostId.generate
    val conflictsWith = implicitly[ConflictsWith[PostEvent]]
    val commentAdded = CommentAdded(postId, CommentId(1), CommentContent("commenter", "body"))
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
        Seq(PostEdited(postId, PostContent(author = "Author", title = "Title", body = "Body")))) must beSome(conflict(PostDeleted(postId)))
    }
  }
}

object PostEventsSpec {
  implicit val arbitraryPostId: Arbitrary[PostId] = Arbitrary(Gen.wrap(PostId.generate()))

  implicit val arbitraryPostContent: Arbitrary[PostContent] = Arbitrary(Gen.resultOf(PostContent.apply _))
  implicit val arbitraryCommentContent: Arbitrary[CommentContent] = Arbitrary(Gen.resultOf(CommentContent.apply _))

  def eventsForSinglePost(id: PostId): Arbitrary[List[PostEvent]] = Arbitrary(for {
    added <- Gen.resultOf(PostAdded(id, _: PostContent))
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
    e <- interleave(edits, comments)
    deleted <- Gen.frequency(3 -> Nil, 1 -> List(PostDeleted(id)))
  } yield added :: e ::: deleted)

  val eventsForMultiplePosts: Arbitrary[List[PostEvent]] = Arbitrary(for {
    events <- Gen.resize(10, Gen.listOf(arbitrary[PostId].flatMap { id => Gen.resize(5, arbitrary(eventsForSinglePost(id))) }))
  } yield events.flatten)

  private[this] def interleave[T](a: List[T], b: List[T]): Gen[List[T]] = {
    if (a.isEmpty) Gen.value(b)
    else if (b.isEmpty) Gen.value(a)
    else for {
      (first, second) <- Gen.oneOf(Seq((a, b), (b, a)))
      rest <- interleave(first.tail, second)
    } yield {
      first.head :: rest
    }
  }
}
