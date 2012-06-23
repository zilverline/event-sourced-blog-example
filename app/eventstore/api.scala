package eventstore

import java.util.UUID

case class CommittedEvent[+A](streamId: UUID, version: Int, committedAt: Long, payload: A) {
  def sequence = version - 1
}

case class Conflict[+A] private[eventstore] (actualVersion: Int, conflictingEvents: Seq[CommittedEvent[A]])
case class Commit[+A] private[eventstore] (sequence: Long, timestamp: Long, events: Seq[CommittedEvent[A]])

trait EventStore[A] {
  def commit[B <: A, R](streamId: UUID, expectedVersion: Int, events: B*)(completed: Either[Conflict[A], Commit[B]] => R): R
}
