package eventstore

import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import Transaction._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class MemoryImageSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  implicit val StringEventStreamType = EventStreamType[String, String](identity, identity)
  implicit val StringConflictsWith = ConflictsWith[String](_ != _)
  type State = Seq[String]
  type Event = String

  "memory image" should {
    "subscribe to event store commits" in new fixture {
      eventStore.committer.tryCommit(Changes("id", StreamRevision.Initial, "event")) must beRight

      subject.get must_== Seq("event")
    }

    "commit modifications to event store" in new fixture {
      subject.modify { state =>
        Changes("id", StreamRevision.Initial, "event").commit(
          onCommit = success,
          onConflict = _ => failure("commit failed"))
      }
      eventStore.reader.readStream("id").flatMap(_.events) must_== Seq("event")
    }

    "fail on unresolvable conflict" in new fixture {
      eventStore.committer.tryCommit(Changes("id", StreamRevision.Initial, "event1")) must beRight

      subject.modify { state =>
        Changes("id", StreamRevision.Initial, "event2").commit(
          onCommit = failure("conflict expected"),
          onConflict = { conflict =>
            conflict.actual must_== StreamRevision(1)
            conflict.events must_== Seq("event1")
          })
      }

      eventStore.reader.readStream("id").flatMap(_.events) must_== Seq("event1")
    }

    "retry on resolved conflict" in new fixture {
      eventStore.committer.tryCommit(Changes("id", StreamRevision.Initial, "event")) must beRight

      subject.modify { state =>
        Changes("id", StreamRevision.Initial, "event").commit(
          onCommit = success,
          onConflict = _ => failure("commit failed"))
      }

      eventStore.reader.readStream("id").flatMap(_.events) must_== Seq("event", "event")
    }

    "retry on transaction conflict" in new fixture {
      val latch = new CountDownLatch(1)

      val first = concurrent.ops.future {
        subject.modify { state =>
          latch.await(5, TimeUnit.SECONDS)
          Changes("id", StreamRevision(state.size), state.size.toString).commit(
            onCommit = success,
            onConflict = _ => failure("commit failed"))
        }
      }

      subject.modify { state =>
        Changes("id", StreamRevision.Initial, state.size.toString).commit(
          onCommit = success,
          onConflict = _ => failure("commit failed"))
      }

      latch.countDown()
      first()

      eventStore.reader.readStream("id").flatMap(_.events) must_== Seq("0", "1")
    }

    "not commit on abort" in new fixture {
      subject.modify { state => abort(success) }

      eventStore.reader.storeRevision must_== StoreRevision.Initial
    }
  }

  trait fixture extends org.specs2.mutable.After {
    val eventStore = new fake.FakeEventStore[Event]
    val subject = MemoryImage[State, Event](eventStore)(Seq.empty) { (state, commit) =>
      state ++ commit.events
    }

    def after {
      eventStore.close
    }
  }
}
