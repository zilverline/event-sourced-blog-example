package events

import org.scalacheck._, Arbitrary.arbitrary, Prop.forAll

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PostEventsSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  import PostEventsSpec._

  "PostId" should {
    "be equal to itself" in forAll { (id: PostId) =>
      id must_== id
    }

    "be unique" in forAll { (a: PostId, b: PostId) =>
      a must_!= b
    }

    "be convertable to and from Strings" in forAll { (id: PostId) =>
      PostId.fromString(id.toString) must beSome(id)
    }

    "fail to parse invalid strings" in forAll { (s: String) =>
      PostId.fromString(s) match {
        case Some(postId) => postId.toString must_== s
        case None         => ok
      }
    }
  }
}

object PostEventsSpec {
  implicit val arbitraryPostId: Arbitrary[PostId] = Arbitrary(Gen.wrap(PostId.generate()))

  implicit val arbitraryPostContent: Arbitrary[PostContent] = Arbitrary(Gen.resultOf(PostContent.apply _))

  def eventsForSinglePost(id: PostId): Arbitrary[List[PostEvent]] = Arbitrary(for {
    added <- Gen.resultOf(PostAdded(id, _: PostContent))
    edits <- Gen.listOf(Gen.resultOf(PostEdited(id, _: PostContent)))
    deleted <- Gen.frequency(3 -> Nil, 1 -> List(PostDeleted(id)))
  } yield added :: edits ::: deleted)

  val eventsForMultiplePosts: Arbitrary[List[PostEvent]] = Arbitrary(for {
    events <- Gen.listOf(arbitrary[PostId].flatMap { id => Gen.resize(5, arbitrary(eventsForSinglePost(id))) })
  } yield events.flatten)
}
