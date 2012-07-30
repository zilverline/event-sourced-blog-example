package eventstore
package redis

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class RedisLuaEventStoreSpec extends EventStoreSpec {
  skipAllIf(!isRedisAvailable)

  def makeEmptyEventStore = new RedisEventStore[String]("spec-" + java.util.UUID.randomUUID, "localhost") with RedisLuaEventCommitter[String] {
    truncate_!

    override def close() = {
      truncate_!
      super.close
    }
  }

  def isRedisAvailable = {
    import _root_.redis.clients.jedis.Jedis
    val jedis = new Jedis("localhost")
    jedis.ping
    jedis.disconnect
    true
  }
}
