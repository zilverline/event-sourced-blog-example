package eventstore

import java.util.UUID
import scala.concurrent.stm._
import org.joda.time.DateTimeUtils

class FakeEventStore[A](onCommit: Commit[A] => Unit) extends EventStore[A] {
  private[this] val commitCount = Ref[Long](0)
  private[this] val stored = TMap.empty[UUID, Seq[CommittedEvent[A]]]

  override def commit[B <: A, R](streamId: UUID, expectedVersion: Int, events: B*)(completed: Either[Conflict[A], Commit[B]] => R): R = {
    val conflictOrCommit = atomic { implicit txn =>
      val existingEvents = stored.get(streamId).getOrElse(Vector.empty)
      val actualVersion = existingEvents.size
      if (expectedVersion != actualVersion) {
        Left(Conflict(actualVersion, existingEvents.drop(expectedVersion)))
      } else {
        val committedAt = DateTimeUtils.currentTimeMillis
        val committedEvents = events.zipWithIndex.map { case (event, index) => CommittedEvent(streamId, actualVersion + index + 1, committedAt, event) }
        stored.put(streamId, existingEvents ++ committedEvents)

        val commitSequence = commitCount()
        commitCount += 1
        val commit = Commit(commitSequence, committedAt, committedEvents)

        onCommit(commit)

        Right(commit)
      }
    }

    completed(conflictOrCommit)
  }
}
