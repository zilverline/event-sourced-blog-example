package eventstore
package fake

import org.joda.time.DateTimeUtils
import scala.actors.threadpool.Executors
import scala.annotation.tailrec
import scala.concurrent.stm._

object FakeEventStore {
  def fromHistory[Event](events: Seq[(String, Event)]): FakeEventStore[Event] = {
    val result = new FakeEventStore[Event]
    for ((streamId, event) <- events) {
      val expected = result.reader.streamRevision(streamId)
      result.committer.tryCommit(streamId, expected, event)
    }
    result
  }
}
class FakeEventStore[Event] extends EventStore[Event] {

  private[this] val executor = Executors.newCachedThreadPool
  private[this] val closed = Ref(false).single
  private[this] val commits = Ref(Vector.empty[Commit[Event]]).single
  private[this] val streams = Ref(Map.empty[String, Vector[Commit[Event]]]).single

  override def toString = "FakeEventStore(" + reader.storeRevision + ")"

  override object reader extends CommitReader[Event] {
    override def storeRevision = StoreRevision(commits().size)

    override def readCommits(since: StoreRevision, to: StoreRevision): Stream[Commit[Event]] = {
      commits().slice((since.value min Int.MaxValue).toInt, (to.value min Int.MaxValue).toInt).toStream
    }

    override def streamRevision(streamId: String) = StreamRevision(streams().get(streamId).map(_.size.toLong).getOrElse(0L))

    override def readStream(streamId: String, since: StreamRevision = StreamRevision.Initial, to: StreamRevision = StreamRevision.Maximum): Stream[Commit[Event]] = {
      streams().getOrElse(streamId, Vector.empty).slice((since.value min Int.MaxValue).toInt, (to.value min Int.MaxValue).toInt).toStream
    }
  }

  override object committer extends EventCommitter[Event] {
    import reader._

    override def tryCommit(streamId: String, expected: StreamRevision, event: Event): CommitResult[Event] = atomic { implicit txn =>
      val actual = streamRevision(streamId)

      if (expected < actual) {
        val conflicting = readStream(streamId, since = expected)
        Left(Conflict(streamId, actual, expected, conflicting))
      } else if (expected > actual) {
        throw new IllegalArgumentException("expected revision %d greater than actual revision %d" format (expected.value, actual.value))
      } else {
        val commit = Commit(storeRevision.next, DateTimeUtils.currentTimeMillis, streamId, actual.next, Seq(event))
        commits.transform(_ :+ commit)
        streams.transform(streams => streams.updated(streamId, streams.getOrElse(streamId, Vector.empty) :+ commit))
        Right(commit)
      }
    }
  }

  override object publisher extends CommitPublisher[Event] {
    override def subscribe(since: StoreRevision)(listener: CommitListener[Event]): Subscription = {
      val cancelled = Ref(false).single
      val last = Ref(since).single

      executor.execute(new Runnable {
        @tailrec override def run {
          val pending = atomic { implicit txn =>
            if (closed() || cancelled()) None else {
              val pending = commits().drop(last().value.toInt)
              if (pending.isEmpty) retry
              else Some(pending)
            }
          }
          pending match {
            case None => // Stop.
            case Some(commits) =>
              commits.foreach { commit =>
                listener(commit)
                last() = commit.storeRevision
              }
              run
          }
        }
      })

      new Subscription {
        override def cancel() = cancelled.set(true)
        override def toString = "Subscription(" + last() + ", " + cancelled() + ", " + FakeEventStore.this + ")"
      }
    }
  }

  def close(): Unit = {
    closed() = true
    executor.shutdown
  }
}
