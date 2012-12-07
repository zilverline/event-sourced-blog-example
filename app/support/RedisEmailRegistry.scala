package support

import _root_.redis.clients.jedis.Jedis
import events.EmailAddress
import events.UserId

object RedisEmailRegistry {
  def claim(jedis: Jedis, redisKey: String): EmailAddress => UserId = { email =>
    val userId = UserId.generate()
    val result: Long = jedis.hsetnx(redisKey, email.toString, userId.toString)
    result match {
      case 0L =>
        val existingUserId = jedis.hget(redisKey, email.toString)
        UserId.fromString(existingUserId).getOrElse(sys.error("cannot parse user id: " + existingUserId))
      case 1L =>
        userId
      case _ =>
        sys.error("unexpected Redis return value: " + result)
    }
  }
}
