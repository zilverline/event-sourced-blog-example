package eventstore

trait ConflictResolver[-Event] {
  def apply(attempted: Event)(committed: Event): Boolean
}
object ConflictResolver {
  def apply[Event](predicate: (Event, Event) => Boolean) = new ConflictResolver[Event] {
    def apply(attempted: Event)(committed: Event): Boolean = predicate(committed, attempted)
  }
}
