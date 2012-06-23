package eventstore

import java.util.UUID
import scala.concurrent.stm._
import org.joda.time.DateTimeUtils

class FakeEventStore[A](onCommit: Commit[A] => Unit) extends EventStore[A] {
  private[this] val commitCount = Ref[Long](0)
  private[this] val stored = TMap.empty[UUID, Seq[CommittedEvent[A]]]

  override def commit[B <: A, R](streamId: UUID, expectedVersion: Int, events: B*)(completed: Either[Conflict[A], Commit[B]] => R): R = {
    val commit = atomic { implicit txn =>
      val existingEvents = stored.get(streamId).getOrElse(Vector.empty)
      val actualVersion = existingEvents.size
      if (expectedVersion != actualVersion) {
        Left(Conflict(actualVersion, existingEvents.drop(expectedVersion)))
      } else {
        val committedAt = DateTimeUtils.currentTimeMillis
        val committedEvents = events.zipWithIndex.map { case (event, index) => CommittedEvent(streamId, existingEvents.size + index + 1, committedAt, event) }
        val commitSequence = commitCount()
        commitCount += 1
        stored.put(streamId, existingEvents ++ committedEvents)
        Right(Commit(commitSequence, committedAt, committedEvents))
      }
    }

    commit.right.foreach(onCommit)
    completed(commit)
  }
}
object FakeEventStore {
  def apply[A](onCommit: Commit[A] => Unit) = new FakeEventStore[A](onCommit)
}
