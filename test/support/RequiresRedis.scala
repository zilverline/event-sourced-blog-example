package support

trait RequiresRedis extends org.specs2.mutable.Specification {
  skipAllIf(!isRedisAvailable)

  def isRedisAvailable = {
    import _root_.redis.clients.jedis.Jedis
    val jedis = new Jedis("localhost")
    jedis.ping
    jedis.disconnect
    true
  }

  trait RedisFixture extends org.specs2.mutable.After {
    import _root_.redis.clients.jedis.Jedis

    val jedis = new Jedis("localhost")

    def after = {
      jedis.disconnect
    }
  }
}
