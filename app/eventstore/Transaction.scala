package eventstore

import scala.annotation.tailrec

case class TransactionContext(transactionRevision: StoreRevision, streamId: String, expected: StreamRevision)
trait Transaction[Event, +A] {
  def run(context: TransactionContext, committer: EventCommitter[Event]): Either[StoreRevision, A]
}
object Transaction {
  def commit[Event, A](event: Event)(onCommit: => A, onConflict: (StreamRevision, Seq[Event]) => A)(implicit resolver: ConflictResolver[Event]): Transaction[Event, A] = new Transaction[Event, A] {
    @tailrec override def run(context: TransactionContext, committer: EventCommitter[Event]): Either[StoreRevision, A] = {
      committer.tryCommit(context.streamId, context.expected, event) match {
        case Right(commit) =>
          Right(onCommit)
        case Left(conflict) =>
          val lastModifiedRevision = conflict.conflicting.last.storeRevision
          if (lastModifiedRevision > context.transactionRevision) {
            Left(lastModifiedRevision)
          } else {
            resolveConflicts(conflict.conflicting.flatMap(_.events), event) match {
              case Left(conflicts) => Right(onConflict(conflict.actual, conflicts))
              case Right(event)    => run(context.copy(expected = conflict.actual), committer)
            }
          }
      }
    }
    private[this] def resolveConflicts(committed: Seq[Event], attempted: Event): Either[Seq[Event], Event] = {
      val conflicting = committed.filter(resolver.apply(_, event))
      if (conflicting.isEmpty) Right(attempted)
      else Left(conflicting)
    }
  }

  def abort[Event, A](value: => A): Transaction[Event, A] = new Transaction[Event, A] {
    override def run(context: TransactionContext, committer: EventCommitter[Event]): Either[StoreRevision, A] = Right(value)
  }
}
