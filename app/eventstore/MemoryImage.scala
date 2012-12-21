package eventstore

import scala.concurrent.stm._

/**
 * The transaction to commit to the event when modifying the memory image.
 */
sealed trait Transaction[+Event, +A] {
  def headers: Map[String, String]
  def withHeaders(headers: (String, String)*): Transaction[Event, A]

  def events: Seq[Event]

  /**
   * Maps the result of this transaction from `A` to `B` using `f`.
   */
  def map[B](f: A => B): Transaction[Event, B]
}
object Transaction {
  /**
   * Transaction result that completes with `onAbort` when run,
   * without committing anything the event store.
   */
  def abort[A](onAbort: => A): Transaction[Nothing, A] = new TransactionAbort(() => onAbort)

  implicit def ChangesOps[Event](changes: Changes[Event]) = new ChangesOps(changes)
  class ChangesOps[Event](changes: Changes[Event]) {
    /**
     * Transaction result that will commit the  `changes` to the event store.
     */
    def commit[A](onCommit: => A, onConflict: Conflict[Event] => A)(implicit conflictsWith: ConflictsWith[Event]): Transaction[Event, A] =
      new TransactionCommit(changes, () => onCommit, onConflict, conflictsWith)
  }
}
private case class TransactionAbort[A](onAbort: () => A, headers: Map[String, String] = Map.empty) extends Transaction[Nothing, A] {
  override def withHeaders(headers: (String, String)*) = copy(headers = this.headers ++ headers)

  override def events = Seq.empty
  override def map[B](f: A => B): Transaction[Nothing, B] = TransactionAbort(() => f(onAbort()))
}
private case class TransactionCommit[Event, A](changes: Changes[Event], onCommit: () => A, onConflict: Conflict[Event] => A, conflictsWith: ConflictsWith[Event]) extends Transaction[Event, A] {
  override def headers = changes.headers
  override def withHeaders(headers: (String, String)*) = copy(changes = changes.withHeaders(headers: _*))
  override def events = changes.events
  override def map[B](f: A => B): Transaction[Event, B] =
    TransactionCommit[Event, B](changes, () => f(onCommit()), conflict => f(onConflict(conflict)), conflictsWith)
}

/**
 * Factory methods for a `MemoryImage`.
 */
object MemoryImage {
  def apply[State, Event: Manifest](eventStore: EventStore[Event])(initialState: State)(update: (State, Commit[Event]) => State) = new MemoryImage(eventStore)(initialState)(update)
}

/**
 * A `MemoryImage` tracks an underlying event store and uses the provided
 * `initialState` and `update` to project the committed events onto the
 * current state.
 */
class MemoryImage[State, -Event: Manifest] private (eventStore: EventStore[Event])(initialState: State)(update: (State, Commit[Event]) => State) {
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
  def modify[A, E <: Event](body: State => Transaction[E, A]): A = {
    @annotation.tailrec def runTransaction(minimum: StoreRevision): A = {
      val (state, transactionRevision) = getWithRevisionAt(minimum)
      body(state) match {
        case TransactionAbort(onAbort, _) =>
          onAbort()
        case TransactionCommit(changes, onCommit, onConflict, conflictsWith) =>
          eventStore.committer.tryCommit(changes) match {
            case Right(commit) =>
              onCommit()
            case Left(conflict) =>
              val conflictRevision = conflict.lastStoreRevision
              if (transactionRevision < conflictRevision) {
                runTransaction(conflictRevision)
              } else {
                conflictsWith.conflicting(conflict, changes.events) match {
                  case Some(conflict) =>
                    onConflict(conflict)
                  case None =>
                    eventStore.committer.tryCommit(changes.withExpectedRevision(conflict.actual)) match {
                      case Right(commit)  => onCommit()
                      case Left(conflict) => runTransaction(conflict.lastStoreRevision)
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
  eventStore.publisher.subscribe[Event](StoreRevision.Initial) { commit =>
    atomic { implicit txn =>
      require(revision().next == commit.storeRevision, "expected: " + revision().next + ", got " + commit.storeRevision)

      state.transform(s => update(s, commit))
      revision() = commit.storeRevision
    }
  }
}
