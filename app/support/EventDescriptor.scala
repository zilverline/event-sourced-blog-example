package support

/**
 * Descriptor to extract the event store stream identifier from an event.
 */
trait EventDescriptor[-Event] {
  def streamId(event: Event): String
}
object EventDescriptor {
  def apply[Event](id: Event => Identifier): EventDescriptor[Event] = new EventDescriptor[Event] {
    override def streamId(event: Event) = id(event).toString
  }
}
