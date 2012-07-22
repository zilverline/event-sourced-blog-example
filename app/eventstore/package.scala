package object eventstore {
  /**
   * The result of a commit attempt is either a `Conflict` or a successful `Commit`.
   */
  type CommitResult[+Event] = Either[Conflict[Event], Commit[Event]]

  /**
   * Event listeners are callback procedures that receive each commit sequentially,
   * without gaps or duplicates.
   */
  type CommitListener[-Event] = Commit[Event] => Unit
}
