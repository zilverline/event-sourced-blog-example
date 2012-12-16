package eventstore

class MemoryImageFixture[StreamId, Event](events: Event*)(implicit descriptor: EventStreamType[StreamId, Event], manifest: Manifest[Event]) {
  val (commits, eventsWithRevision, streamRevisions) = {
    val eventStore = fake.FakeEventStore.fromHistory(events)
    try {
      val commits = eventStore.reader.readCommits[Event](StoreRevision.Initial, StoreRevision.Maximum)
      val eventsWithRevision = commits.flatMap(_.eventsWithRevision).toList
      val streamRevisions = eventsWithRevision.groupBy(p => descriptor.streamId(p._1)).map(p => p._1 -> p._2.last._2)
      (commits, eventsWithRevision, streamRevisions)
    } finally {
      eventStore.close
    }
  }
}
