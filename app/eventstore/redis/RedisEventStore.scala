package eventstore
package redis

import java.util.UUID
import java.util.concurrent.Executors
import org.apache.commons.pool.impl.GenericObjectPool.Config
import org.joda.time.DateTimeUtils
import play.api.Logger
import play.api.libs.json._
import _root_.redis.clients.jedis._
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import support.EventStreamType

object RedisEventStore {
  val DEFAULT_PORT = Protocol.DEFAULT_PORT

  def apply[Event: Format](name: String, host: String, port: Int = DEFAULT_PORT, config: Config = new Config, disableLua: Boolean = false): RedisEventStore[Event] = {
    if (disableLua || !supportsLua(host, port)) {
      Logger.warn("Redis Lua scripting is disabled or not supported, falling back to using WATCH/MULTI/EXEC.")
      new RedisEventStore(name, host, port, config) with RedisWatchMultiExecEventCommitter[Event]
    } else {
      Logger.info("Redis Lua scripting is supported and enabled, using Lua event committer script.")
      new RedisEventStore(name, host, port, config) with RedisLuaEventCommitter[Event]
    }
  }

  private[this] def supportsLua(host: String, port: Int): Boolean = {
    val jedis = new Jedis(host, port)
    try {
      // Detect if Redis suports Lua by trying to execute the SCRIPT EXISTS command.
      jedis.scriptExists(); true
    } catch {
      case _: exceptions.JedisDataException => false
    } finally {
      jedis.disconnect()
    }
  }
}

/**
 * Defines the `reader` and `publisher` implementations for an `EventStore`. The committer implementations can be mixed in at instantiation time.
 *
 * The Redis event store implementation uses two primary data structures:
 *
 * - A hash containing all commits indexed by the store revision (commit id).
 * - A list of commit ids per event stream.
 *
 * Events most have an associated `Format` instance to allow for (de)serialization to JSON.
 */
abstract class RedisEventStore[Event] protected (name: String, host: String, port: Int = RedisEventStore.DEFAULT_PORT, config: Config = new Config)(implicit val eventFormat: Format[Event]) extends EventStore[Event] {

  // Each event store has its own namespace in Redis, based on the event store name.
  protected[this] val KeyPrefix = name + ":"

  // Redis key for the commits hash. Also used as commits publication channel for subscribers.
  protected[this] val CommitsKey: String = KeyPrefix + "commits"

  protected[this] val StreamKeyPrefix: String = KeyPrefix + "stream:"

  // Redis key for the list of commit ids per event stream.
  protected[this] def keyForStream(streamId: String): String = StreamKeyPrefix + streamId

  // Executor for event store subscribers. Each subscriber gets its own thread.
  private[this] val executor = Executors.newCachedThreadPool

  // Subscriber control channel for handling event store closing and subscription cancellation.
  private[this] val ControlChannel: String = KeyPrefix + "control"

  // Control message used to notify subscribers the event store is closing.
  private[this] val CloseToken = UUID.randomUUID.toString
  @volatile private[this] var closed = false

  // Redis connection pool for readers and committers. Subscribers do not use this pool.
  private[this] val jedisPool = new JedisPool(config, host, port)

  // Execute a Redis command with a connection from the pool.
  protected[this] def withJedis[A](f: Jedis => A): A = {
    val jedis = jedisPool.getResource
    try {
      f(jedis)
    } finally {
      jedisPool.returnResource(jedis: BinaryJedis)
    }
  }

  // Maximum number of commits to read at one time.
  private[this] val ChunkSize = 10000

  // Reads and deserializes commits in `ChunkSize` chunks.
  private[this] def doReadCommits(commitIds: Seq[String]): Stream[Commit[Event]] = {
    val chunks = commitIds.grouped(ChunkSize).map(_.toArray)
    chunks.flatMap { chunk =>
      val serializedCommits = withJedis { _.hmget(CommitsKey, chunk: _*) }
      serializedCommits.asScala.par.map(deserializeCommit)
    }.toStream
  }

  // Helpers to serialize and deserialize commits.
  protected[this] def deserializeCommit(serialized: String): Commit[Event] = Json.fromJson[Commit[Event]](Json.parse(serialized))
  protected[this] def serializeCommit(commit: Commit[Event]): String = Json.stringify(Json.toJson(commit))

  override object reader extends CommitReader[Event] {
    override def storeRevision: StoreRevision = withJedis { jedis => StoreRevision(jedis.hlen(CommitsKey)) }

    override def readCommits[E <: Event: Manifest](since: StoreRevision, to: StoreRevision): Stream[Commit[E]] = {
      val current = storeRevision
      if (since >= current) Stream.empty else {
        val revisionRange = (since.value + 1) to (to.value min current.value)
        doReadCommits(revisionRange.map(_.toString)).map(_.withOnlyEventsOfType[E])
      }
    }

    override def streamRevision[Id, Event](streamId: Id)(implicit descriptor: EventStreamType[Id, Event]): StreamRevision = withJedis { jedis =>
      StreamRevision(jedis.llen(keyForStream(descriptor.toString(streamId))))
    }

    override def readStream[Id, E <: Event](streamId: Id, since: StreamRevision = StreamRevision.Initial, to: StreamRevision = StreamRevision.Maximum)(implicit descriptor: EventStreamType[Id, E]): Stream[Commit[E]] = {
      val commitIds = withJedis { _.lrange(keyForStream(descriptor.toString(streamId)), since.value, to.value) }
      doReadCommits(commitIds.asScala).map(commit => commit.copy(events = commit.events.map(descriptor.cast)))
    }
  }

  override object publisher extends CommitPublisher[Event] {
    import reader._

    override def subscribe[E <: Event: Manifest](since: StoreRevision)(listener: CommitListener[E]): Subscription = {
      @volatile var cancelled = false
      @volatile var last = since
      val unsubscribeToken = UUID.randomUUID.toString

      executor.execute(new Runnable {
        private def replayCommitsTo(to: StoreRevision) {
          if (last < to) {
            Logger.info("Replaying commits since " + last + " to " + to)
            readCommits(last, to).takeWhile(_ => !closed && !cancelled).foreach(listener)
            last = to
          }
        }

        private object Subscriber extends JedisPubSub {
          override def onSubscribe(channel: String, subscribedChannels: Int) = channel match {
            case ControlChannel =>
              // We may have missed the cancellation token while subscribing, so check the flag.
              if (closed || cancelled) unsubscribe
            case CommitsKey =>
              // We may have missed some commits while subscribing, so replay missing if needed.
              replayCommitsTo(storeRevision)
            case _ =>
              Logger.warn("message received on unknown channel '" + channel + "'")
          }

          override def onMessage(channel: String, message: String) = channel match {
            case ControlChannel =>
              if (message == CloseToken || message == unsubscribeToken) {
                unsubscribe
              }
            case CommitsKey =>
              val commit = deserializeCommit(message)
              if (last.next < commit.storeRevision) {
                Logger.warn("missing commits since " + last + " to " + commit.storeRevision + ", replaying...")
                replayCommitsTo(commit.storeRevision)
              } else if (last.next == commit.storeRevision) {
                listener(commit.withOnlyEventsOfType[E])
                last = commit.storeRevision
              } else {
                Logger.warn("Ignoring old commit " + commit.storeRevision + ", since we already processed everything up to " + last)
              }
            case _ =>
              Logger.warn("message received on unknown channel '" + channel + "'")
          }

          override def onPMessage(pattern: String, channel: String, message: String) {}
          override def onUnsubscribe(channel: String, subscribedChannels: Int) {}
          override def onPSubscribe(pattern: String, subscribedChannels: Int) {}
          override def onPUnsubscribe(pattern: String, subscribedChannels: Int) {}
        }

        override def run {
          val currentRevision = storeRevision
          if (last > currentRevision) {
            Logger.warn("Last " + last + " is in the future, resetting it to current " + currentRevision)
            last = currentRevision
          } else {
            replayCommitsTo(currentRevision)
          }

          val jedis = new Jedis(host, port)
          try {
            jedis.subscribe(Subscriber, ControlChannel, CommitsKey)
          } finally {
            jedis.disconnect
          }
        }
      })

      new Subscription {
        override def cancel() = {
          cancelled = true
          withJedis { _.publish(ControlChannel, unsubscribeToken) }
        }

        override def toString = "Subscription(" + last + ", " + cancelled + ", " + RedisEventStore.this + ")"
      }
    }
  }

  /**
   * Deletes all data from the event store. Use carefully!
   */
  def truncate_!(): Unit = withJedis { jedis: BinaryJedis =>
    val keys = jedis.keys((KeyPrefix + "*").getBytes("UTF-8"))
    keys.asScala.grouped(ChunkSize).foreach { group => jedis.del(group.toArray: _*) }
  }

  /**
   * Closes the event store. All subscribers will be unsubscribed and connections to Redis will be closed.
   */
  def close(): Unit = {
    closed = true
    withJedis { _.publish(ControlChannel, CloseToken) }
    executor.shutdown
    jedisPool.destroy
  }

  override def toString = "RedisEventStore(%s, %s:%d)".format(name, host, port)
}
