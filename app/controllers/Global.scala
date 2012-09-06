package controllers

import events._
import eventstore._
import models._
import org.apache.commons.pool.impl.GenericObjectPool.Config
import org.joda.time.DateTimeUtils
import play.api._
import play.api.libs.json._
import play.api.Play.current

object Global extends GlobalSettings {
  object persistence {
    private val config = Play.configuration.getConfig("eventstore").getOrElse(throw Play.configuration.globalError("missing [eventstore] configuration"))

    implicit val DomainEventFormat: Format[DomainEvent] = PostEvent.PostEventFormat and UserEvent.UserEventFormat

    val eventStore = config.getString("implementation", Some(Set("fake", "redis"))).get match {
      case "fake" =>
        new fake.FakeEventStore[DomainEvent]
      case "redis" =>
        val jedisConfig = new Config
        jedisConfig.minIdle = config.getInt("redis.connection-pool-size").getOrElse(8)
        jedisConfig.maxIdle = jedisConfig.minIdle
        jedisConfig.maxActive = jedisConfig.minIdle
        redis.RedisEventStore[DomainEvent](
          config.getString("redis.prefix").getOrElse(throw config.globalError("missing key [eventstore.redis.prefix]")),
          config.getString("redis.host").getOrElse(throw config.globalError("missing key [eventstore.redis.host]")),
          config.getInt("redis.port").getOrElse(redis.RedisEventStore.DEFAULT_PORT),
          disableLua = config.getBoolean("redis.disable-lua").getOrElse(false),
          config = jedisConfig)
    }

    val memoryImage = MemoryImage[State, DomainEvent](eventStore)(State()) {
      (state, commit) => state.updateMany(commit.eventsWithRevision)
    }
  }

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
    Logger.info("Loaded memory image with %d commits in %.3f seconds (%.1f commits/second)".format(commitCount, elapsed, commitCount / elapsed))
  }
}
