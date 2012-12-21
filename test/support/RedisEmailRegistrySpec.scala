package support

import java.util.UUID
import redis.clients.jedis.Jedis
import events.EmailAddress

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class RedisEmailRegistrySpec extends org.specs2.mutable.Specification with RequiresRedis {

  "Redis email registry" should {
    "map same email address to same user id" in new fixture {
      val userId = subject(EmailAddress("john@example.com"))
      subject(EmailAddress("john@example.com")) must_== userId
    }

    "map different email address to different user ids" in new fixture {
      val userId = subject(EmailAddress("john@example.com"))
      subject(EmailAddress("joe@example.com")) must_!= userId
    }
  }

  trait fixture extends RedisFixture {
    val redisKey = "spec-" + UUID.randomUUID

    val subject = RedisEmailRegistry.claim(jedis, redisKey)

    override def after = {
      jedis.del(redisKey)
      super.after
    }
  }
}
