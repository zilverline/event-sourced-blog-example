package support

trait ConflictsWith[-Event] {
  def apply(committed: Event, attempted: Event): Boolean
}
object ConflictsWith {
  def apply[Event](predicate: (Event, Event) => Boolean) = new ConflictsWith[Event] {
    def apply(committed: Event, attempted: Event): Boolean = predicate(committed, attempted)
  }
}
