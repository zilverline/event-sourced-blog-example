package support

import _root_.redis.clients.jedis.{ Jedis, JedisPool, JedisPoolConfig, Protocol }
import scala.collection.JavaConverters._

trait RequiresRedis extends org.specs2.mutable.Specification {
  val RedisHost = "localhost"
  val RedisPort = Protocol.DEFAULT_PORT
  val RedisPoolConfig = {
    val config = new JedisPoolConfig()
    config.setMaxActive(50)
    config
  }

  skipAllUnless(isRedisAvailable)

  def isRedisAvailable = {
    val jedis = new Jedis(RedisHost, RedisPort)
    jedis.ping
    jedis.disconnect
    true
  }

  def newJedisPool = new JedisPool(RedisPoolConfig, RedisHost, RedisPort)

  trait RedisFixture extends org.specs2.mutable.After {
    val prefix = s"spec-${java.util.UUID.randomUUID}"
    lazy val jedis = new Jedis(RedisHost, RedisPort)

    def after = {
      jedis.keys(s"$prefix*").asScala foreach { jedis.del(_) }
      jedis.disconnect
    }
  }
}
