package support

import events._
import java.util.UUID
import redis.clients.jedis.Jedis

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class RedisEmailRegistrySpec extends Spec with RequiresRedis {

  "Redis email registry" should {
    "map new email address to requested user id" in new fixture {
      val requestedUserId = UserId.generate
      val returnedUserId = subject.claim(EmailAddress("john@example.com"), requestedUserId)
      returnedUserId must_== requestedUserId
    }

    "map existing email address to previous user id" in new fixture {
      val existingUserId = subject.claim(EmailAddress("john@example.com"), UserId.generate)
      val returnedUserId = subject.claim(EmailAddress("john@example.com"), UserId.generate)
      returnedUserId must_== existingUserId
    }
  }

  trait fixture extends RedisFixture {
    val redisKey = "spec-" + UUID.randomUUID

    val subject = new RedisEmailRegistry(jedis, redisKey)

    override def after = {
      jedis.del(redisKey)
      super.after
    }
  }
}
