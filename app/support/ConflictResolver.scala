package support

trait ConflictResolver[-Event] {
  def apply(committed: Event, attempted: Event): Boolean
}
object ConflictResolver {
  def apply[Event](predicate: (Event, Event) => Boolean) = new ConflictResolver[Event] {
    def apply(committed: Event, attempted: Event): Boolean = predicate(committed, attempted)
  }
}
