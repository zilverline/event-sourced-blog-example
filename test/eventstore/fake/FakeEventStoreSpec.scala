package eventstore
package fake

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class FakeEventStoreSpec extends EventStoreSpec {
  def makeEmptyEventStore = new FakeEventStore[String]
}
