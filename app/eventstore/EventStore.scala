package eventstore

import play.api.libs.json._
import support.JsonMapping._
import support.EventStreamType

/**
 * The revision of an event store. The revision of an event store is
 * equal to the number of commits in the event store.
 */
final case class StoreRevision(value: Long) extends Ordered[StoreRevision] {
  require(value >= 0, "store revision cannot be negative")

  def previous = StoreRevision(value - 1)
  def next = StoreRevision(value + 1)

  def +(that: Long): StoreRevision = StoreRevision(value + that)
  def -(that: Long): StoreRevision = StoreRevision(value - that)
  def -(that: StoreRevision): Long = this.value - that.value

  override def compare(that: StoreRevision) = value compare that.value
}
object StoreRevision {
  val Initial = StoreRevision(0)
  val Maximum = StoreRevision(Long.MaxValue)

  implicit val StoreRevisionFormat: Format[StoreRevision] = valueFormat(apply)(unapply)
}

/**
 * The revision of an event stream. The revision of an event stream is
 * equal to the number of commits in the event stream.
 */
final case class StreamRevision(value: Long) extends Ordered[StreamRevision] {
  require(value >= 0, "stream revision cannot be negative")

  def previous = StreamRevision(value - 1)
  def next = StreamRevision(value + 1)

  def +(that: Long): StreamRevision = StreamRevision(value + that)
  def -(that: Long): StreamRevision = StreamRevision(value - that)
  def -(that: StreamRevision): Long = this.value - that.value

  override def compare(that: StreamRevision) = value compare that.value
}
object StreamRevision {
  val Initial = StreamRevision(0)
  val Maximum = StreamRevision(Long.MaxValue)

  implicit val StreamRevisionFormat: Format[StreamRevision] = valueFormat(apply)(unapply)
}

/**
 * Represents the changes that can be committed atomically to the event store.
 */
sealed trait Changes[Event] {
  type StreamId
  def eventStreamType: EventStreamType[StreamId, Event]

  def streamId: StreamId
  def expected: StreamRevision
  def events: Seq[Event]

  def withExpectedRevision(expected: StreamRevision): Changes[Event]
}
object Changes {
  def apply[StreamId, Event](streamId: StreamId, expected: StreamRevision, events: Event*)(implicit streamType: EventStreamType[StreamId, Event]): Changes[Event] =
    new EventStreamChanges(streamId, expected, events, streamType)
  def apply[StreamId, Event](expected: StreamRevision, event: Event)(implicit streamType: EventStreamType[StreamId, Event]): Changes[Event] =
    apply(streamType.streamId(event), expected, event)
}
private[this] case class EventStreamChanges[A, Event](streamId: A, expected: StreamRevision, events: Seq[Event], eventStreamType: EventStreamType[A, Event]) extends Changes[Event] {
  type StreamId = A

  override def withExpectedRevision(expected: StreamRevision) = copy(expected = expected)
}

/**
 * A successful commit to `streamId`.
 */
case class Commit[+Event](storeRevision: StoreRevision, timestamp: Long, streamId: String, streamRevision: StreamRevision, events: Seq[Event]) {
  def eventsWithRevision: Seq[(Event, StreamRevision)] = events.map(event => (event, streamRevision))

  def withOnlyEventsOfType[E](implicit manifest: Manifest[E]): Commit[E] = copy(events = events.collect {
    case event if manifest.erasure.isInstance(event) => event.asInstanceOf[E]
  })

  def committedEvents: Seq[CommittedEvent[Event]] = events.map { event =>
    CommittedEvent(storeRevision, timestamp, streamId, streamRevision, event)
  }
}
object Commit {
  implicit def CommitFormat[Event: Format]: Format[Commit[Event]] = objectFormat("storeRevision", "timestamp", "streamId", "streamRevision", "events")(apply[Event] _)(unapply)
}

/**
 * An event together with its commit related information.
 */
case class CommittedEvent[+Event](storeRevision: StoreRevision, timestamp: Long, streamId: String, streamRevision: StreamRevision, event: Event)

/**
 * The conflict that occurred while trying to commit to `streamId`.
 */
case class Conflict[+Event](committedEvents: Seq[CommittedEvent[Event]]) {
  require(committedEvents.nonEmpty, "committedEvents.nonEmpty")

  def streamId = committedEvents.head.streamId
  def actual = committedEvents.last.streamRevision
  def events = committedEvents.map(_.event)

  def lastStoreRevision = committedEvents.last.storeRevision

  def filter(predicate: CommittedEvent[Event] => Boolean): Option[Conflict[Event]] = {
    val filtered = committedEvents.filter(predicate)
    if (filtered.isEmpty) None else Some(Conflict(filtered))
  }
}

/**
 * Reads commits from the event store.
 */
trait CommitReader[-Event] {
  /**
   * The current store revision.
   */
  def storeRevision: StoreRevision

  /**
   * Reads all commits `since` (exclusive) up `to` (inclusive). Events are
   * filtered to only include events of type `E`.
   */
  def readCommits[E <: Event](since: StoreRevision, to: StoreRevision)(implicit manifest: Manifest[E]): Stream[Commit[E]]

  /**
   * The current stream revision of the stream identified by `streamId`.
   */
  def streamRevision[StreamId, Event](streamId: StreamId)(implicit descriptor: EventStreamType[StreamId, Event]): StreamRevision

  /**
   * Reads all commits from the stream identified by `streamId` that occurred
   * `since` (exclusive) up `to` (inclusive).
   *
   * @throws ClassCastException the stream contained commits that did not have
   *         the correct type `E`.
   */
  def readStream[StreamId, E <: Event](streamId: StreamId, since: StreamRevision = StreamRevision.Initial, to: StreamRevision = StreamRevision.Maximum)(implicit descriptor: EventStreamType[StreamId, E]): Stream[Commit[E]]
}

/**
 * Commits events to an event store.
 */
trait EventCommitter[-Event] {
  def tryCommit[E <: Event](changes: Changes[E]): CommitResult[E]
}

/**
 * A subscription that can be cancelled.
 */
trait Subscription {
  def cancel(): Unit
}

/**
 * Publishes successful commits to subscribers.
 */
trait CommitPublisher[-Event] {
  /**
   * Notifies `listener` of all commits that happened `since`. Notification happens asynchronously.
   */
  def subscribe[E <: Event: Manifest](since: StoreRevision)(listener: CommitListener[E]): Subscription
}

/**
 * The event store API.
 */
trait EventStore[-Event] {
  /**
   * The commit reader associated with this event store.
   */
  def reader: CommitReader[Event]

  /**
   * The event committer associated with this event store.
   */
  def committer: EventCommitter[Event]

  /**
   * The commit publisher associated with this event store.
   */
  def publisher: CommitPublisher[Event]

  /**
   * Closes the event store. All subscribers are automatically unsubscribed.
   */
  def close(): Unit
}

class EventStoreException(message: String, cause: Throwable) extends RuntimeException(message, cause)
