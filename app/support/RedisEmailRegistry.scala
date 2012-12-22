package support

import _root_.redis.clients.jedis.Jedis
import events.EmailAddress
import events.UserId

class RedisEmailRegistry(jedis: Jedis, redisKey: String) {
  def claim(email: EmailAddress, requestedUserId: UserId): UserId = {
    val result: Long = jedis.hsetnx(redisKey, email.toString, requestedUserId.toString)
    result match {
      case 0L =>
        val existingUserId = jedis.hget(redisKey, email.toString)
        UserId.fromString(existingUserId).getOrElse(sys.error(s"cannot parse user id: $existingUserId"))
      case 1L =>
        requestedUserId
      case _ =>
        sys.error(s"unexpected Redis return value: $result")
    }
  }
}
