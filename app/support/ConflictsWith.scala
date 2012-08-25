package support

import eventstore.Conflict

/**
 * Compares committed events against an attempted events to check for
 * conflicts.
 */
trait ConflictsWith[-Event] {
  /**
   * Checks each committed event from `conflict` for conflicts with the `attempted` events.
   * Any committed event that conflicts is returned.
   */
  def conflicting[A <: Event, B <: Event](conflict: Conflict[A], attempted: Seq[B]): Option[Conflict[A]]
}
object ConflictsWith {
  /**
   * Builds a new `ConflictsWith` based on the `conflicts` predicate.
   */
  def apply[Event](conflicts: (Event, Event) => Boolean) = new ConflictsWith[Event] {
    override def conflicting[A <: Event, B <: Event](conflict: Conflict[A], attempted: Seq[B]): Option[Conflict[A]] =
      conflict.filter(a => attempted.exists(b => conflicts(a.event, b)))
  }
}
