package eventstore

import scala.concurrent.stm._
import scala.annotation.tailrec
import support.ConflictResolver

sealed trait Transaction[+Event, +A]
private case class TransactionAbort[A](onAbort: () => A) extends Transaction[Nothing, A]
private case class TransactionCommit[Event, A](append: Update[Event], onCommit: () => A, onConflict: (StreamRevision, Seq[Event]) => A) extends Transaction[Event, A]

object Transaction {
  def commit[Event, A](append: Update[Event])(onCommit: => A, onConflict: (StreamRevision, Seq[Event]) => A): Transaction[Event, A] =
    TransactionCommit(append, () => onCommit, onConflict)

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
  def get: State = getWithRevisionAt(eventStore.reader.storeRevision)._1

  /**
   * Runs the provided `body` against this event store and attempts to commit
   * the produced event. The transaction is automatically retried when a write
   * conflict is detected, so the provided `body` must be side-effect free.
   */
  def modify[A](body: State => Transaction[Event, A])(implicit resolver: ConflictResolver[Event]): A = {
    @tailrec def runTransaction(minimum: StoreRevision): A = {
      val (state, transactionRevision) = getWithRevisionAt(minimum)
      body(state) match {
        case TransactionAbort(onAbort) =>
          onAbort()
        case TransactionCommit(append, onCommit, onConflict) =>
          eventStore.committer.tryCommit(append) match {
            case Right(commit) =>
              onCommit()
            case Left(conflict) =>
              val conflictRevision = conflict.commits.last.storeRevision
              if (transactionRevision < conflictRevision) {
                runTransaction(conflictRevision)
              } else {
                val conflicting = conflict.events.filter(resolver(_, append.event))
                if (conflicting.nonEmpty) {
                  onConflict(conflict.actual, conflicting)
                } else {
                  eventStore.committer.tryCommit(append) match {
                    case Right(commit)  => onCommit()
                    case Left(conflict) => runTransaction(conflictRevision)
                  }
                }
              }
          }
      }
    }

    runTransaction(eventStore.reader.storeRevision)
  }

  override def toString = "MemoryImage(%s, %s)".format(revision.single.get, eventStore)

  private[this] def getWithRevisionAt(minimum: StoreRevision): (State, StoreRevision) = atomic { implicit txn =>
    if (revision() < minimum) retry
    else (state(), revision())
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
