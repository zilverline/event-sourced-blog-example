package support

import java.util.UUID
import redis.clients.jedis.Jedis
import events.EmailAddress

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class RedisEmailRegistrySpec extends org.specs2.mutable.Specification {
  skipAllIf(!isRedisAvailable)

  val subject = RedisEmailRegistry.claim(new Jedis("localhost"), "spec-" + UUID.randomUUID)

  "Redis email registry" should {
    "map same email address to same user id" in {
      val userId = subject(EmailAddress("john@example.com"))

      subject(EmailAddress("john@example.com")) must_== userId
      subject(EmailAddress("joe@example.com")) must_!= userId
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
