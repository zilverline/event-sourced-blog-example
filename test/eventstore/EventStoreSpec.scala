package eventstore

import java.util.UUID
import java.util.concurrent.{ CountDownLatch, TimeUnit }
import org.joda.time.DateTimeUtils
import org.scalacheck._, Arbitrary.arbitrary, Prop.{ forAll, forAllNoShrink }
import play.api.libs.json._

object EventStoreSpec {
  private[this] implicit def arbitrarySeq[A: Arbitrary]: Arbitrary[Seq[A]] = Arbitrary(arbitrary[List[A]])

  implicit def arbitraryStoreRevision: Arbitrary[StoreRevision] = Arbitrary(Gen.chooseNum(StoreRevision.Initial.value, StoreRevision.Maximum.value).map(StoreRevision.apply))
  implicit def arbitraryStreamRevision: Arbitrary[StreamRevision] = Arbitrary(Gen.chooseNum(StreamRevision.Initial.value, StreamRevision.Maximum.value).map(StreamRevision.apply))

  implicit def arbitraryCommit[Event: Arbitrary]: Arbitrary[Commit[Event]] = Arbitrary(Gen.resultOf(Commit.apply[Event] _))
  implicit def arbitraryConflict[Event: Arbitrary]: Arbitrary[Conflict[Event]] = Arbitrary(Gen.resultOf(Conflict.apply[Event] _))
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
    "support ordering - transitivity" in check(forAll { (a: StoreRevision, b: StoreRevision, c: StoreRevision) =>
      (a < b && b < c) ==> a < c
    })(set('minTestsOk -> 10))
    "support ordering - totality" in forAll { (a: StoreRevision, b: StoreRevision) =>
      a <= b || b <= a
    }

    "support JSON serialization" in forAll { (a: StoreRevision) =>
      Json.fromJson[StoreRevision](Json.toJson(a)) must_== a
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
    "support ordering - transitivity" in check(forAll { (a: StreamRevision, b: StreamRevision, c: StreamRevision) =>
      (a < b && b < c) ==> a < c
    })(set('minTestsOk -> 10))
    "support ordering - totality" in forAll { (a: StreamRevision, b: StreamRevision) =>
      a <= b || b <= a
    }

    "support JSON serialization" in forAll { (a: StreamRevision) =>
      Json.fromJson[StreamRevision](Json.toJson(a)) must_== a
    }
  }
}

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConflictAndCommitSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  val SerializedCommit = """{"storeRevision":5,"timestamp":1342542931694,"streamId":"StreamId","streamRevision":2,"events":["Event1","Event2"]}"""
  val ExampleCommit = Commit(StoreRevision(5), 1342542931694L, "StreamId", StreamRevision(2), Seq("Event1", "Event2"))

  val SerializedConflict = """{"streamId":"StreamId","actual":2,"expected":1,"conflicting":[%s]}""".format(SerializedCommit)
  val ExampleConflict = Conflict("StreamId", actual = StreamRevision(2), expected = StreamRevision(1), conflicting = Seq(ExampleCommit))

  "Commits" should {
    "deserialize example JSON" in {
      Json.fromJson[Commit[String]](Json.parse(SerializedCommit)) must_== ExampleCommit
    }

    "be serializable to and from JSON" in forAll { (commit: Commit[String]) =>
      Json.fromJson[Commit[String]](Json.toJson(commit)) must_== commit
    }

    "combine event with stream revision" in {
      ExampleCommit.eventsWithRevision must_== Seq(("Event1", StreamRevision(2)), ("Event2", StreamRevision(2)))
    }
  }

  "Conflicts" should {
    "deserialize example JSON" in {
      Json.fromJson[Conflict[String]](Json.parse(SerializedConflict)) must_== ExampleConflict
    }

    "be serializable to and from JSON" in forAll { (conflict: Conflict[String]) =>
      Json.fromJson[Conflict[String]](Json.toJson(conflict)) must_== conflict
    }
  }
}

trait EventStoreSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  val streamIdGenerator = Gen.wrap(UUID.randomUUID.toString)

  "An event store" should {
    val id = Gen.alphaStr.sample.get
    val event = Gen.alphaStr.sample.get
    val event1 = Gen.alphaStr.sample.get
    val event2 = Gen.alphaStr.sample.get

    "commit initial event to stream" in new fixture {
      val result = subject.committer.tryCommit(id, StreamRevision.Initial, event)

      result must_== Right(Commit(StoreRevision(1), now, id, StreamRevision(1), Seq(event)))
      subject.reader.storeRevision must_== StoreRevision.Initial.next
    }

    "detect and return conflicting events" in new fixture {
      subject.committer.tryCommit(id, StreamRevision.Initial, event1)

      val result = subject.committer.tryCommit(id, StreamRevision.Initial, event2)

      result must_== Left(Conflict(
        streamId = id,
        actual = StreamRevision.Initial.next,
        expected = StreamRevision.Initial,
        conflicting = Seq(Commit(StoreRevision(1), now, id, StreamRevision(1), Seq(event1)))))
    }

    "store commits" in new fixture {
      subject.committer.tryCommit("streamId", StreamRevision(0), "event")

      subject.reader.readStream("streamId") must_== Seq(Commit(StoreRevision(1), now, "streamId", StreamRevision(1), Seq("event")))
    }

    "store commits in multiple streams" in new fixture {
      checkProp(forAll(Gen.listOf(streamIdGenerator)) { streamIds =>
        val startRevision = subject.reader.storeRevision
        for (streamId <- streamIds) {
          val currentRevision = subject.reader.storeRevision
          subject.committer.tryCommit(streamId, StreamRevision.Initial, "event") must beRight
          subject.reader.storeRevision must_== currentRevision.next
        }

        subject.reader.readCommits(startRevision, StoreRevision.Maximum).map(_.streamId) must_== streamIds
      })
    }

    "not store conflicts" in new fixture {
      subject.committer.tryCommit(id, StreamRevision.Initial, event1)
      subject.committer.tryCommit(id, StreamRevision.Initial, event2)

      subject.reader.readStream(id) must_== Seq(Commit(StoreRevision(1), now, id, StreamRevision(1), Seq(event1)))
    }

    "notify subscriber of commits" in new fixture {
      checkProp(forAllNoShrink(Gen.listOf(streamIdGenerator)) { streamIds =>
        val startRevision = subject.reader.storeRevision

        val countDown = new CountDownLatch(streamIds.size)
        val notifications = Vector.newBuilder[Commit[String]]
        val subscription = subject.publisher.subscribe(startRevision) { commit =>
          notifications += commit
          countDown.countDown
        }

        commitMany(streamIds)

        countDown.await(2, TimeUnit.SECONDS) aka "the notification completed on time" must beTrue
        subscription.cancel()

        notifications.result must_== subject.reader.readCommits(startRevision, StoreRevision.Maximum)
      })
    }

    "replay commits when subscribing" in new fixture {
      checkProp(forAllNoShrink(Gen.listOf(streamIdGenerator)) { streamIds =>
        val startRevision = subject.reader.storeRevision

        commitMany(streamIds)

        val countDown = new CountDownLatch(streamIds.size)
        val notifications = Vector.newBuilder[Commit[String]]

        val subscription = subject.publisher.subscribe(startRevision) { commit =>
          notifications += commit
          countDown.countDown
        }

        countDown.await(2, TimeUnit.SECONDS) aka "the notification completed on time" must beTrue
        subscription.cancel()

        notifications.result must_== subject.reader.readCommits(startRevision, StoreRevision.Maximum)
      })
    }
  }

  def makeEmptyEventStore: EventStore[String]

  trait fixture extends org.specs2.mutable.After {
    val now = DateTimeUtils.currentTimeMillis
    DateTimeUtils.setCurrentMillisFixed(now)

    val subject = makeEmptyEventStore

    def commitMany(streamIds: List[String]) {
      for (streamId <- streamIds) {
        subject.committer.tryCommit(streamId, StreamRevision.Initial, "event") must beRight
      }
    }

    def after {
      DateTimeUtils.setCurrentMillisSystem
      subject.close
    }
  }
}
