package eventstore
package redis

import _root_.redis.clients.jedis._
import scala.collection.JavaConverters._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class RedisEventStoreSpec extends EventStoreSpec[RedisEventStore] with support.RequiresRedis {
  implicit val jedisPool = newJedisPool

  def makeEmptyEventStore = new RedisEventStore[String]("spec-" + java.util.UUID.randomUUID)

  "The redis event store" should {
    genericEventStoreExamples(makeEmptyEventStore)

    "enqueue commits to the registered Redis queues" in new fixture(makeEmptyEventStore) with RedisFixture {
      override def after = { super[fixture].after; super[RedisFixture].after }

      val queueKey = s"${subject.name}:queue"

      subject.addDispatchQueue(queueKey)

      subject.committer.tryCommit(Changes(StreamRevision.Initial, "abcdef")) must beRight

      jedis.lrange(queueKey, 0, -1).asScala must_== Seq("1")
    }
  }

  step {
    jedisPool.destroy()
  }
}
