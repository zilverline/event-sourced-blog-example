package eventstore

import scala.concurrent.stm._
import scala.annotation.tailrec

/**
 * Factory methods for a `MemoryImage`
 */
object MemoryImage {
  def apply[State, Event](eventStore: EventStore[Event])(initialState: State)(update: (State, Commit[Event]) => State) = new MemoryImage(eventStore)(initialState)(update)
}

/**
 * A `MemoryImage` tracks an underlying event store and uses the provided
 * `initialState` and `update` to project the committed events onto the
 * current state.
 */
class MemoryImage[State, Event] private (eventStore: EventStore[Event])(initialState: State)(update: (State, Commit[Event]) => State) extends EventCommitter[Event] {
  memoryImage =>

  private[this] val state = Ref(initialState)
  private[this] val revision = Ref(StoreRevision.Initial)

  /**
   * The current state of the memory image with at least all commits applied
   * that have been committed to the underlying event store.
   */
  def get: State = get(eventStore.reader.storeRevision)

  def get(minimum: StoreRevision): State = atomic { implicit txn =>
    if (revision() < minimum) retry
    else state()
  }

  /**
   * Commits an event to the underlying event store. The memory image will be
   * updated if the commit succeeds.
   */
  override def tryCommit(streamId: String, expected: StreamRevision, event: Event): CommitResult[Event] = eventStore.committer.tryCommit(streamId, expected, event)

  def modify[Id, Aggregate](id: Id, expected: StreamRevision)(implicit reader: (State, Id) => Option[(StreamRevision, Aggregate)]) = new {
    def apply[A, E <: Event](body: Option[Aggregate] => Transaction[Event, A]): A = {
      @tailrec def loop(expected: StreamRevision, state: State): A = {
        val (actual, aggregate) = reader(state, id) match {
          case Some((r, a)) => (r, Some(a))
          case None         => (eventStore.reader.streamRevision(id.toString), None)
        }
        val transaction = body(aggregate)
        transaction.run(TransactionContext(id.toString, expected, actual))(memoryImage) match {
          case Left(storeRevision) => loop(expected, memoryImage.get(storeRevision))
          case Right(a)            => a
        }
      }
      loop(expected, memoryImage.get)
    }
  }

  override def toString = "MemoryImage(%s, %s)".format(revision.single.get, eventStore)

  // Subscribe to the underlying event store and apply every commit to the
  // current state using the provided `update` function.
  eventStore.publisher.subscribe(StoreRevision.Initial) { commit =>
    atomic { implicit txn =>
      require(revision().next == commit.storeRevision, "expected: " + revision().next + ", got " + commit.storeRevision)

      state.transform(s => update(s, commit))
      revision() = commit.storeRevision
    }
  }
}
