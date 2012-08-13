package support

/**
 * Compares committed events against an attempted events to check for
 * conflicts.
 */
trait ConflictsWith[-Event] {
  /**
   * Checks each `committed` event for conflicts against all `attempted` events.
   * Any committed event that conflicts is returned.
   */
  def conflicting[A <: Event, B <: Event](committed: Seq[A], attempted: Seq[B]): Seq[A]
}
object ConflictsWith {
  /**
   * Builds a new `ConflictsWith` based on the given predicate.
   */
  def apply[Event](predicate: (Event, Event) => Boolean) = new ConflictsWith[Event] {
    override def conflicting[A <: Event, B <: Event](committed: Seq[A], attempted: Seq[B]): Seq[A] =
      committed.filter(a => attempted.exists(b => predicate(a, b)))
  }
}
