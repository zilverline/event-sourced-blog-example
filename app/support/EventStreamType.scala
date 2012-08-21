package support

/**
 * Event stream type information.
 */
trait EventStreamType[Id, Event] {
  /**
   * Extract stream identifier from `event`.
   */
  def streamId(event: Event): Id

  /**
   * Convert a stream identifier to a string. Used by the event store to persist the stream identifier.
   */
  def toString(id: Id): String

  /**
   * Cast `event` to the `Event` type.
   *
   * @throws ClassCastException if `event` is not of type `Event`.
   */
  def cast(event: Any): Event
}
object EventStreamType {
  def apply[Id, Event](eventToStreamId: Event => Id, writeStreamId: Id => String)(implicit manifest: Manifest[Event]) = new EventStreamType[Id, Event] {
    def streamId(event: Event) = eventToStreamId(event)
    def toString(id: Id) = writeStreamId(id)

    def cast(event: Any): Event = {
      if (manifest.erasure.isInstance(event)) event.asInstanceOf[Event]
      else throw new ClassCastException(event + " is not an instance of " + manifest)
    }
  }
}
