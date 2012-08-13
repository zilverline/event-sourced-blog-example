package support

/**
 * Predicate that can check a committed event against an attempted event to
 * check for conflicts.
 */
trait ConflictsWith[-Event] {
  /**
   * Checks each `committed` event for conflicts against all `attempted` events.
   * Any committed event that conflicts is returned.
   */
  def conflicting[Committed <: Event, Attempted <: Event](committed: Seq[Committed], attempted: Seq[Attempted]): Seq[Committed]
}
object ConflictsWith {
  /**
   * Builds a new `ConflictsWith` based on the given predicate.
   */
  def apply[Event](predicate: (Event, Event) => Boolean) = new ConflictsWith[Event] {
    override def conflicting[Committed <: Event, Attempted <: Event](committed: Seq[Committed], attempted: Seq[Attempted]): Seq[Committed] =
      committed.filter(a => attempted.exists(b => predicate(a, b)))
  }
}
