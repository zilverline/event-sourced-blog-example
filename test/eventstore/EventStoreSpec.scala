package eventstore

import java.util.UUID
import java.util.concurrent.{ CountDownLatch, TimeUnit }
import org.joda.time.DateTimeUtils
import org.scalacheck._, Arbitrary.arbitrary, Prop.{ forAll, forAllNoShrink }
import org.specs2.matcher.Matcher
import play.api.libs.json._

object EventStoreSpec {
  private[this] implicit def arbitrarySeq[A: Arbitrary]: Arbitrary[Seq[A]] = Arbitrary(arbitrary[List[A]])

  implicit def arbitraryStoreRevision: Arbitrary[StoreRevision] = Arbitrary(Gen.chooseNum(StoreRevision.Initial.value, StoreRevision.Maximum.value).map(StoreRevision.apply))
  implicit def arbitraryStreamRevision: Arbitrary[StreamRevision] = Arbitrary(Gen.chooseNum(StreamRevision.Initial.value, StreamRevision.Maximum.value).map(StreamRevision.apply))

  implicit def arbitraryCommit[Event: Arbitrary]: Arbitrary[Commit[Event]] = Arbitrary(Gen.resultOf(Commit.apply[Event] _))
}
import EventStoreSpec._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class StoreRevisionSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  "Store revisions" should {
    "have lower and upper bounds" in {
      StoreRevision.Initial.previous must throwA[IllegalArgumentException]
      StoreRevision.Maximum.next must throwA[IllegalArgumentException]
    }

    "know previous and next values" in forAll { (a: StoreRevision) =>
      (a > StoreRevision.Initial && a < StoreRevision.Maximum) ==> {
        a.next must_== (a + 1)
        a.previous must_== (a - 1)
        a.next.previous must_== a
        a.previous.next must_== a
      }
    }

    "support difference and addition" in forAll { (a: StoreRevision, b: StoreRevision) =>
      a + (b - a) must_== b
    }

    "support ordering" in forAll { (a: StoreRevision) =>
      (a > StoreRevision.Initial && a < StoreRevision.Maximum) ==> {
        a.previous must be_<(a)
        a.next must not be_< (a)
      }
    }

    "support ordering - equality" in forAll { (a: StoreRevision) =>
      a == a && a <= a && a >= a
    }

    "support ordering - transitivity" in prop { (a: StoreRevision, b: StoreRevision, c: StoreRevision) =>
      val Seq(x, y, z) = Seq(a, b, c).sorted
      (x < y && y < z) ==> x < z
    }

    "support ordering - totality" in forAll { (a: StoreRevision, b: StoreRevision) =>
      a <= b || b <= a
    }

    "support JSON serialization" in forAll { (a: StoreRevision) =>
      Json.fromJson[StoreRevision](Json.toJson(a)) must_== JsSuccess(a)
    }
  }
}

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class StreamRevisionSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  "Stream revisions" should {
    "have lower and upper bounds" in {
      StreamRevision.Initial.previous must throwA[IllegalArgumentException]
      StreamRevision.Maximum.next must throwA[IllegalArgumentException]
    }

    "know previous and next values" in forAll { (a: StreamRevision) =>
      (a > StreamRevision.Initial && a < StreamRevision.Maximum) ==> {
        a.next must_== (a + 1)
        a.previous must_== (a - 1)
        a.next.previous must_== a
        a.previous.next must_== a
      }
    }

    "support difference and addition" in forAll { (a: StreamRevision, b: StreamRevision) =>
      a + (b - a) must_== b
    }

    "support ordering" in forAll { (a: StreamRevision) =>
      (a > StreamRevision.Initial && a < StreamRevision.Maximum) ==> {
        a.previous must be_<(a)
        a.next must not be_< (a)
      }
    }

    "support ordering - equality" in forAll { (a: StreamRevision) =>
      a == a && a <= a && a >= a
    }

    "support ordering - transitivity" in prop { (a: StreamRevision, b: StreamRevision, c: StreamRevision) =>
      val Seq(x, y, z) = Seq(a, b, c).sorted
      (x < y && y < z) ==> x < z
    }

    "support ordering - totality" in forAll { (a: StreamRevision, b: StreamRevision) =>
      a <= b || b <= a
    }

    "support JSON serialization" in forAll { (a: StreamRevision) =>
      Json.fromJson[StreamRevision](Json.toJson(a)) must_== JsSuccess(a)
    }
  }
}

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class CommitSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  val SerializedCommit = """{"storeRevision":5,"timestamp":1342542931694,"streamId":"StreamId","streamRevision":2,"events":["Event1","Event2"],"headers":{"header":"value"}}"""
  val ExampleCommit = Commit(StoreRevision(5), 1342542931694L, "StreamId", StreamRevision(2), Seq("Event1", "Event2"), Map("header" -> "value"))

  "Commits" should {
    "deserialize example JSON" in {
      Json.fromJson[Commit[String]](Json.parse(SerializedCommit)) must_== JsSuccess(ExampleCommit)
    }

    "be serializable to and from JSON" in forAll { (commit: Commit[String]) =>
      Json.fromJson[Commit[String]](Json.toJson(commit)) must_== JsSuccess(commit)
    }

    "combine event with stream revision" in {
      ExampleCommit.eventsWithRevision must_== Seq(("Event1", StreamRevision(2)), ("Event2", StreamRevision(2)))
    }

    "filter events based on type" in {
      forAll { (commit: Commit[String]) => commit.withOnlyEventsOfType[String] must_== commit }
      forAll { (commit: Commit[String]) => commit.withOnlyEventsOfType[Any]    must_== commit }
      forAll { (commit: Commit[String]) => commit.withOnlyEventsOfType[UUID]   must_== commit.copy(events = Seq.empty) }
    }
  }
}

trait EventStoreSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  val streamIdGenerator = Gen.wrap(UUID.randomUUID.toString)
  implicit val StringEventStreamType: EventStreamType[String, String] = EventStreamType(identity, identity)

  def matchCommit[A] = (be_==(_: Commit[A])) ^^^ ((_:Commit[A]).copy(timestamp = 0))

  "An event store" should {
    val id = Gen.alphaStr.sample.get
    val event = Gen.alphaStr.sample.get
    val event1 = Gen.alphaStr.sample.get
    val event2 = Gen.alphaStr.sample.get

    "commit initial event to stream" in new fixture {
      val result = subject.committer.tryCommit(Changes(id, StreamRevision.Initial, event))

      result must beRight
      result.right.get must matchCommit(Commit(StoreRevision(1), now, id, StreamRevision(1), Seq(event), Map.empty))

      subject.reader.storeRevision must_== StoreRevision.Initial.next
    }

    "detect and return conflicting events" in new fixture {
      subject.committer.tryCommit(Changes(id, StreamRevision.Initial, event1))

      val result = subject.committer.tryCommit(Changes(id, StreamRevision.Initial, event2))

      result must beLeft
      result.left.get.streamId must_== id
      result.left.get.actual must_== StreamRevision(1)
      result.left.get.events must have size (1)
      val commit = result.left.get.committedEvents(0)
      commit.timestamp must be >= now
      commit.storeRevision must_== StoreRevision(1)
      commit.event must_== event1
    }

    "store commits" in new fixture {
      subject.committer.tryCommit(Changes("streamId", StreamRevision(0), "event").withHeaders("header" -> "value"))

      subject.reader.streamRevision("streamId") must_== StreamRevision(1)
      val commits = subject.reader.readStream("streamId")
      commits must have size(1)
      commits(0) must matchCommit(Commit(StoreRevision(1), now, "streamId", StreamRevision(1), Seq("event"), Map("header" -> "value")))
    }

    "store commits in multiple streams" in new fixture {
      checkProp(forAll(Gen.listOf(streamIdGenerator)) { streamIds =>
        val startRevision = subject.reader.storeRevision
        for (streamId <- streamIds) {
          val currentRevision = subject.reader.storeRevision
          subject.committer.tryCommit(Changes(streamId, StreamRevision.Initial, "event")) must beRight
          subject.reader.storeRevision must_== currentRevision.next
        }

        subject.reader.readCommits(startRevision, StoreRevision.Maximum).map(_.streamId) must_== streamIds
      })
    }

    "not store conflicts" in new fixture {
      subject.committer.tryCommit(Changes(id, StreamRevision.Initial, event1))
      subject.committer.tryCommit(Changes(id, StreamRevision.Initial, event2))

      subject.reader.streamRevision(id) must_== StreamRevision(1)
    }

    "notify subscriber of commits" in new fixture {
      checkProp(forAllNoShrink(Gen.listOf(streamIdGenerator)) { streamIds =>
        val startRevision = subject.reader.storeRevision

        val countDown = new CountDownLatch(streamIds.size)
        val notifications = Vector.newBuilder[Commit[String]]
        val subscription = subject.publisher.subscribe[String](startRevision) { commit =>
          notifications += commit
          countDown.countDown
        }

        commitMany(streamIds)

        countDown.await(2, TimeUnit.SECONDS) aka "the notification completed on time" must beTrue
        subscription.cancel()

        notifications.result must_== subject.reader.readCommits[String](startRevision, StoreRevision.Maximum)
      })
    }

    "replay commits when subscribing" in new fixture {
      checkProp(forAllNoShrink(Gen.listOf(streamIdGenerator)) { streamIds =>
        val startRevision = subject.reader.storeRevision

        commitMany(streamIds)

        val countDown = new CountDownLatch(streamIds.size)
        val notifications = Vector.newBuilder[Commit[String]]

        val subscription = subject.publisher.subscribe[String](startRevision) { commit =>
          notifications += commit
          countDown.countDown
        }

        countDown.await(2, TimeUnit.SECONDS) aka "the notification completed on time" must beTrue
        subscription.cancel()

        notifications.result must_== subject.reader.readCommits[String](startRevision, StoreRevision.Maximum)
      })
    }
  }

  def makeEmptyEventStore: EventStore[String]

  trait fixture extends org.specs2.mutable.After {
    val now = DateTimeUtils.currentTimeMillis

    val subject = makeEmptyEventStore

    def commitMany(streamIds: List[String]) {
      for (streamId <- streamIds) {
        subject.committer.tryCommit(Changes(streamId, StreamRevision.Initial, "event")) must beRight
      }
    }

    def after {
      subject.close
    }
  }
}
