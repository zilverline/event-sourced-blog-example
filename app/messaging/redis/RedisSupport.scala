package messaging.redis

import play.api.Logger
import _root_.redis.clients.jedis._
import scala.concurrent.blocking

trait RedisSupport {
  private[this] val logger = Logger(classOf[RedisSupport])

  protected[this] trait RedisPubSubNoOps { this: JedisPubSub =>
    override def onSubscribe(channel: String, subscribedChannels: Int) {}
    override def onMessage(channel: String, message: String) {}
    override def onPMessage(pattern: String, channel: String, message: String) {}
    override def onUnsubscribe(channel: String, subscribedChannels: Int) {}
    override def onPSubscribe(pattern: String, subscribedChannels: Int) {}
    override def onPUnsubscribe(pattern: String, subscribedChannels: Int) {}
  }

  protected def jedisPool: JedisPool

  // Execute a Redis command with a connection from the pool.
  protected[this] def withJedis[A](f: Jedis => A): A = blocking {
    val jedis = jedisPool.getResource
    try {
      f(jedis)
    } finally {
      jedisPool.returnResource(jedis: BinaryJedis)
    }
  }

  protected[this] def subscribeToChannels(jedisPubSub: JedisPubSub)(channels: String*): Unit = blocking {
    val jedis = jedisPool.getResource
    try {
      jedis.subscribe(jedisPubSub, channels: _*)
    } finally {
      jedisPool.returnBrokenResource(jedis: BinaryJedis)
    }
  }

  protected class LuaScript(val script: String) {
    val id = withJedis { _.scriptLoad(script) }
    logger.trace(s"Loaded Lua script $script with id $id")

    def eval(keys: String*)(args: String*) = withJedis { _.evalsha(id, keys.size, keys ++ args: _*) }
  }
}
