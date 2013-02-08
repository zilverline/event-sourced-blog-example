package messaging

import _root_.redis.clients.jedis._
import org.apache.commons.pool.impl.GenericObjectPool.Config
import scala.concurrent._
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import akka.actor.ActorSystem
import play.api.Logger
import org.joda.time.DateTimeUtils
import org.joda.time.Instant
import akka.event.ActorEventBus
import akka.event.LookupClassification
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import scala.util.Try

sealed trait Protocol
case class Process(messageId: String, timesOutAt: Instant) extends Protocol
//case class TimedOut(messageId: String, timedOutAt: Long) extends Protocol

sealed trait MonitoringProtocol
case class ProcessingStarted(messageId: String, timesOutAt: Instant) extends MonitoringProtocol
case class ProcessingTimedOut(messageId: String, timedOutAt: Instant) extends MonitoringProtocol
case class ProcessingCompleted(messageId: String, completedAt: Instant) extends MonitoringProtocol

object RedisQueue {
  val NamePattern = "[A-Za-z0-9_-]+"
}
class RedisQueue(
  val name: String,
  val initialTimeout: FiniteDuration = 30.seconds)(
    process: String => Future[Unit])(implicit actorSystem: ActorSystem, val jedisPool: JedisPool)
    extends redis.RedisSupport {

  require(name matches RedisQueue.NamePattern, s"queue name incorrect: $name")

  private val logger = Logger(classOf[RedisQueue])

  private implicit val dispatcher = actorSystem.dispatcher

  val ControlChannel = s"$name:control"
  val IncomingQueueKey = s"$name:incoming"
  val PendingQueueKey = s"$name:pending"
  val ProcessingSortedSetKey = s"$name:processing"

  @volatile private var closed = false
  private val shutdownLatch = new CountDownLatch(2)
  private val ShutdownToken = "SHUTDOWN " + java.util.UUID.randomUUID

  def enqueue(messageId: String) {
    withJedis { _.lpush(IncomingQueueKey, messageId) }
    ()
  }

  object monitoring extends ActorEventBus with LookupClassification {
    type Classifier = Unit
    type Event = MonitoringProtocol

    def subscribe(subscriber: Subscriber): Boolean = subscribe(subscriber, ())

    override protected def classify(event: Event) = ()
    override protected def mapSize(): Int = 1

    override protected def publish(event: Event, subscriber: Subscriber) = subscriber ! event
  }

  private object ProcessPendingScript extends LuaScript("""
    | local pendingQueueKey = KEYS[1]
    | local processingSortedSetKey = KEYS[2]
    | local controlChannel = KEYS[3]
    | local initialTimeout = ARGV[1]
    | for _, messageId in ipairs(redis.call('lrange', pendingQueueKey, 0, -1)) do
    |   redis.call('zadd', processingSortedSetKey, initialTimeout, messageId)
    |   redis.call('publish', controlChannel, 'STARTED ' .. messageId .. ' ' .. initialTimeout)
    | end
    | redis.call('del', pendingQueueKey)
    """.stripMargin) {
    def apply(initialTimeout: Instant) {
      eval(PendingQueueKey, ProcessingSortedSetKey, ControlChannel)(initialTimeout.getMillis.toString)
      ()
    }
  }

  private object TimedOutScript extends LuaScript("""
    | local processingSortedSetKey = KEYS[1]
    | local controlChannel = KEYS[2]
    | local now = ARGV[1]
    | for _, messageId in ipairs(redis.call('zrangebyscore', processingSortedSetKey, 0, now)) do
    |   redis.call('zrem', processingSortedSetKey, messageId)
    |   redis.call('publish', controlChannel, 'TIMEDOUT ' .. messageId .. ' ' .. now)
    | end
    """.stripMargin) {
    def apply(now: Instant) {
      eval(ProcessingSortedSetKey, ControlChannel)(now.getMillis.toString)
      ()
    }
  }

  private object CompletedScript extends LuaScript("""
    | local processingSortedSetKey = KEYS[1]
    | local controlChannel = KEYS[2]
    | local now = ARGV[1]
    | local messageId = ARGV[2]
    | if redis.call('zrem', processingSortedSetKey, messageId) > 0 then
    |   redis.call('publish', controlChannel, 'COMPLETED ' .. messageId .. ' ' .. now)
    | end
    """.stripMargin) {
    def apply(now: Instant, messageId: String) {
      eval(ProcessingSortedSetKey, ControlChannel)(now.getMillis.toString, messageId)
      ()
    }
  }

  future {
    logger.info(s"Listening to $IncomingQueueKey")
    @annotation.tailrec def run {
      val messageId = Option(withJedis { _.brpoplpush(IncomingQueueKey, PendingQueueKey, 1) })

      val timesOutAt = new Instant() plus initialTimeout.toMillis
      ProcessPendingScript(timesOutAt)

      messageId foreach { messageId =>
        logger.trace(s"Processing $messageId from $IncomingQueueKey")
        process(messageId).onComplete { result =>
          CompletedScript(new Instant(), messageId)
          logger.trace(s"Completed $messageId with $result")
        }
      }
      if (!closed) run
    }
    run
  } onComplete { result =>
    shutdownLatch.countDown()
    logger.info(s"Stopped listening to $IncomingQueueKey: $result")
  }

  private val timeoutChecker = actorSystem.scheduler.schedule(initialDelay = 1.seconds, interval = 10.seconds min initialTimeout) {
    TimedOutScript(new Instant())
  }

  object ControlChannelSubscriber extends JedisPubSub with RedisPubSubNoOps {
    private val Started = s"STARTED (${RedisQueue.NamePattern}) ([0-9]+)".r
    private val Completed = s"COMPLETED (${RedisQueue.NamePattern}) ([0-9]+)".r
    private val TimedOut = s"TIMEDOUT (${RedisQueue.NamePattern}) ([0-9]+)".r
    override def onMessage(channel: String, message: String) = message match {
      case Started(messageId, timesOutAt) =>
        monitoring publish ProcessingStarted(messageId, new Instant(timesOutAt.toLong))
      case Completed(messageId, completedAt) =>
        monitoring publish ProcessingCompleted(messageId, new Instant(completedAt.toLong))
      case TimedOut(messageId, timedOutAt) =>
        monitoring publish ProcessingTimedOut(messageId, new Instant(timedOutAt.toLong))
      case ShutdownToken =>
        unsubscribe
      case message =>
        logger.warn(s"$name: bad control message: $message")
    }
  }
  future[Unit] {
    subscribeToChannels(ControlChannelSubscriber)(ControlChannel)
    shutdownLatch.countDown()
  }

  def close() {
    closed = true
    timeoutChecker.cancel()
    withJedis { _.publish(ControlChannel, ShutdownToken) }
    shutdownLatch.await(5, TimeUnit.SECONDS)
    ()
  }
}
