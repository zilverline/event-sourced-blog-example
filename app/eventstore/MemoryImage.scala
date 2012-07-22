package eventstore

import scala.concurrent.stm._

/**
 * Factory methods for a `MemoryImage`
 */
object MemoryImage {
  def apply[State, Event](eventStore: EventStore[Event])(initialState: State)(update: (State, Commit[Event]) => State) = new MemoryImage(eventStore)(initialState)(update)
}

/**
 * A `MemoryImage` tracks an underlying event store and uses the provided
 * `initialState` and `update` to project the committed events unto the
 * current state.
 */
class MemoryImage[State, Event] private (eventStore: EventStore[Event])(initialState: State)(update: (State, Commit[Event]) => State) extends EventCommitter[Event] {
  private[this] val state = Ref(initialState)
  private[this] val revision = Ref(StoreRevision.Initial)

  /**
   * The current state of the memory image with at least all commits applied
   * that have been committed to the underlying event store.
   */
  def get: State = {
    val minimum = eventStore.reader.storeRevision
    atomic { implicit txn =>
      if (revision() < minimum) retry
      else state()
    }
  }

  /**
   * Commits an event to the underlying event store. The memory image will be
   * updated if the commit succeeds.
   */
  def tryCommit(streamId: String, expected: StreamRevision, event: Event): CommitResult[Event] = eventStore.committer.tryCommit(streamId, expected, event)

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
