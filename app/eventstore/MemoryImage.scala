package eventstore

import scala.concurrent.stm._
import scala.annotation.tailrec

sealed trait Transaction[+Event, +A]
private case class TransactionAbort[A](onAbort: () => A) extends Transaction[Nothing, A]
private case class TransactionCommit[Event, A](event: Event, onCommit: () => A, onConflict: (StreamRevision, Seq[Event]) => A, conflictResolver: ConflictResolver[Event]) extends Transaction[Event, A]

object Transaction {
  def commit[Event, A](event: Event)(onCommit: => A, onConflict: (StreamRevision, Seq[Event]) => A)(implicit resolver: ConflictResolver[Event]): Transaction[Event, A] =
    TransactionCommit(event, () => onCommit, onConflict, resolver)

  def abort[A](value: => A): Transaction[Nothing, A] = TransactionAbort(() => value)
}

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
class MemoryImage[State, Event] private (eventStore: EventStore[Event])(initialState: State)(update: (State, Commit[Event]) => State) {
  private[this] val state = Ref(initialState)
  private[this] val revision = Ref(StoreRevision.Initial)

  /**
   * The current state of the memory image with at least all commits applied
   * that have been committed to the underlying event store.
   */
  def get: State = get(eventStore.reader.storeRevision)._2

  /**
   * Runs the provided `body` against this event store and attempts to commit
   * the produced event. The transaction is automatically retried when a write
   * conflict is detected, so the provided `body` must be side-effect free.
   */
  def modify[A](streamId: String, expected: StreamRevision)(body: State => Transaction[Event, A]): A = {
    @tailrec def runTransaction(minimum: StoreRevision): A = {
      val (transactionRevision, state) = get(minimum)
      body(state) match {
        case TransactionAbort(onAbort) =>
          onAbort()
        case TransactionCommit(event, onCommit, onConflict, resolver) =>
          eventStore.committer.tryCommit(streamId, expected, event) match {
            case Right(commit) =>
              onCommit()
            case Left(conflict) =>
              if (transactionRevision < conflict.lastModifiedRevision) {
                runTransaction(conflict.lastModifiedRevision)
              } else {
                val conflicts = conflict.events.filter(resolver(event))
                if (conflicts.nonEmpty) {
                  onConflict(conflict.actual, conflicts)
                } else {
                  eventStore.committer.tryCommit(streamId, conflict.actual, event) match {
                    case Right(commit)  => onCommit()
                    case Left(conflict) => runTransaction(conflict.conflicting.last.storeRevision)
                  }
                }
              }
          }
      }
    }

    runTransaction(eventStore.reader.storeRevision)
  }

  override def toString = "MemoryImage(%s, %s)".format(revision.single.get, eventStore)

  private[this] def get(minimum: StoreRevision): (StoreRevision, State) = atomic { implicit txn =>
    if (revision() < minimum) retry
    else (revision(), state())
  }

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
