package controllers

import akka.actor.ActorSystem
import events._
import eventstore._
import messaging._
import models._
import org.apache.commons.pool.impl.GenericObjectPool.Config
import org.joda.time.DateTimeUtils
import play.api._
import play.api.libs.json._
import play.api.Play.current
import _root_.redis.clients.jedis.{Protocol, Jedis, JedisPool, JedisPoolConfig}
import support.RedisEmailRegistry
import scala.concurrent._

object Global extends GlobalSettings {
  implicit val actorSystem = ActorSystem("blog")

  object persistence {
    private val config = Play.configuration.getConfig("eventstore").getOrElse(throw Play.configuration.globalError("missing [eventstore] configuration"))

    implicit val DomainEventFormat: Format[DomainEvent] = PostEvent.PostEventFormat and UserEvent.UserEventFormat

    val eventStore = config.getString("implementation", Some(Set("fake", "redis"))).get match {
      case "fake" =>
        new fake.FakeEventStore[DomainEvent]
      case "redis" =>
        val jedisConfig = new JedisPoolConfig
        jedisConfig.setMinIdle(config.getInt("redis.connection-pool-size").getOrElse(8))
        jedisConfig.setMaxIdle(jedisConfig.minIdle)
        jedisConfig.setMaxActive(jedisConfig.minIdle)
        val redisHost = config.getString("redis.host").getOrElse(throw config.globalError("missing key [eventstore.redis.host]"))
        val redisPort = config.getInt("redis.port").getOrElse(Protocol.DEFAULT_PORT)
        implicit val jedisPool = new JedisPool(jedisConfig, redisHost, redisPort)
        val es = new eventstore.redis.RedisEventStore[DomainEvent](
          config.getString("redis.prefix").getOrElse(throw config.globalError("missing key [eventstore.redis.prefix]")))
        val queue = new RedisQueue("myqueue")(messageId => {
          Logger.debug(s"Received $messageId for processing")
          Future.successful(())
        })
        queue.startProcessing
        es.addDispatchQueue(queue.IncomingQueueKey)
        es
    }

    val memoryImage = MemoryImage[ApplicationState, DomainEvent](eventStore)(ApplicationState()) {
      (state, commit) => state.updateMany(commit.eventsWithRevision)
    }
  }

  lazy val emailAddressRegistry: RedisEmailRegistry = {
    val config = Play.configuration.getConfig("email-registry").getOrElse(throw Play.configuration.globalError("missing [email-registry] configuration"))

    val redisHost = config.getString("redis.host").getOrElse(throw config.globalError("missing key [email-registry.redis.host]"))
    val redisPort = config.getInt("redis.port").getOrElse(Protocol.DEFAULT_PORT)
    val redisKey = config.getString("redis.key").getOrElse(throw config.globalError("missing key [email-registry.redis.key]"))

    val jedis = new Jedis(redisHost, redisPort)

    new RedisEmailRegistry(jedis, redisKey)
  }

  object MemoryImageActions extends MemoryImageActions(persistence.memoryImage)

  override def onStart(app: Application) {
    initializeMemoryImage
  }

  private def initializeMemoryImage {
    val start = DateTimeUtils.currentTimeMillis

    import persistence._
    val commitCount = eventStore.reader.storeRevision.value
    memoryImage.get // Waits for event replay to complete.

    val stop = DateTimeUtils.currentTimeMillis
    val elapsed = (stop - start) / 1000.0
    Logger.info(f"Loaded memory image with $commitCount%d commits in $elapsed%.3f seconds (${commitCount / elapsed}%.1f commits/second)")
  }
}
