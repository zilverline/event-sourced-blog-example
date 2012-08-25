package support

/**
 * Event stream type information.
 */
trait EventStreamType[StreamId, Event] {
  /**
   * Convert a stream identifier to a string. Used by the event store to persist the stream identifier.
   */
  def toString(streamId: StreamId): String

  /**
   * Extract stream identifier from `event`.
   */
  def streamId(event: Event): StreamId

  /**
   * Cast `event` to the `Event` type.
   *
   * @throws ClassCastException if `event` is not of type `Event`.
   */
  def cast(event: Any): Event
}
object EventStreamType {
  def apply[StreamId, Event](writeStreamId: StreamId => String, eventToStreamId: Event => StreamId)(implicit manifest: Manifest[Event]) = new EventStreamType[StreamId, Event] {
    def toString(streamId: StreamId) = writeStreamId(streamId)
    def streamId(event: Event) = eventToStreamId(event)

    def cast(event: Any): Event = {
      if (manifest.erasure.isInstance(event)) event.asInstanceOf[Event]
      else throw new ClassCastException(event + " is not an instance of " + manifest)
    }
  }
}
