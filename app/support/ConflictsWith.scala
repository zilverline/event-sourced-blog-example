package support

/**
 * Predicate that can check a committed event against an attempted event to
 * check for conflicts.
 */
trait ConflictsWith[-Event] {
  /**
   * True if the `committed` event conflicts with the `attempted` event, false
   * otherwise.
   */
  def apply(committed: Event, attempted: Event): Boolean

  /**
   * Checks each `committed` event for conflicts against all `attempted` events.
   * Any committed event that conflicts is returned.
   */
  def conflicting[E <: Event](committed: Seq[E], attempted: Seq[E]): Seq[E] =
    committed.filter(a => attempted.exists(b => apply(a, b)))
}
object ConflictsWith {
  def apply[Event](predicate: (Event, Event) => Boolean) = new ConflictsWith[Event] {
    override def apply(committed: Event, attempted: Event): Boolean = predicate(committed, attempted)
  }
}
