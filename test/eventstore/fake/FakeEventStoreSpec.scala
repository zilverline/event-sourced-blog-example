package eventstore
package fake

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class FakeEventStoreSpec extends EventStoreSpec[FakeEventStore] {
  "The fake event store" should {
    genericEventStoreExamples(new FakeEventStore[String])
  }
}
