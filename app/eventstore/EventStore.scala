package eventstore

import play.api.libs.json._
import support.EventDescriptor
import support.JsonMapping._

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
 * Represents an event that can be committed to a stream.
 */
case class Update[+Event](streamId: String, expected: StreamRevision, event: Event)
object Update {
  def apply[Event](expected: StreamRevision, event: Event)(implicit descriptor: EventDescriptor[Event]): Update[Event] =
    Update(descriptor.streamId(event), expected, event)

  implicit def pairToUpdate[Event: EventDescriptor](p: (StreamRevision, Event)): Update[Event] = Update(p._1, p._2)
}

/**
 * A successful commit to `streamId`.
 */
case class Commit[+Event](storeRevision: StoreRevision, timestamp: Long, streamId: String, streamRevision: StreamRevision, events: Seq[Event]) {
  def eventsWithRevision: Seq[(Event, StreamRevision)] = events.map(event => (event, streamRevision))
}
object Commit {
  implicit def CommitFormat[Event: Format]: Format[Commit[Event]] = objectFormat("storeRevision", "timestamp", "streamId", "streamRevision", "events")(apply[Event] _)(unapply)
}

/**
 * The conflict that occurred while trying to commit to `streamId`.
 */
case class Conflict[+Event](streamId: String, actual: StreamRevision, expected: StreamRevision, commits: Seq[Commit[Event]]) {
  require(actual > expected, "actual > expected")
  require(commits.nonEmpty, "commits.nonEmpty")

  def events: Seq[Event] = commits.flatMap(_.events)
}
object Conflict {
  implicit def ConflictFormat[Event: Format]: Format[Conflict[Event]] = objectFormat("streamId", "actual", "expected", "conflicting")(apply[Event] _)(unapply)
}

/**
 * Reads commits from the event store.
 */
trait CommitReader[Event] {
  def storeRevision: StoreRevision
  def readCommits(since: StoreRevision, to: StoreRevision): Stream[Commit[Event]]
  def streamRevision(streamId: String): StreamRevision
  def readStream(streamId: String, since: StreamRevision = StreamRevision.Initial, to: StreamRevision = StreamRevision.Maximum): Stream[Commit[Event]]
}

/**
 * Commits events to an event store.
 */
trait EventCommitter[Event] {
  def tryCommit(append: Update[Event]): CommitResult[Event]
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
trait CommitPublisher[Event] {
  /**
   * Notifies `listener` of all commits that happened `since`. Notification happens asynchronously.
   */
  def subscribe(since: StoreRevision)(listener: CommitListener[Event]): Subscription
}

/**
 * The event store API.
 */
trait EventStore[Event] {
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
