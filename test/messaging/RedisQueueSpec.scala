package messaging

import akka.actor.ActorSystem
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import scala.concurrent._
import scala.concurrent.duration._
import akka.actor.Props
import akka.actor.Actor

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class RedisQueueSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck with support.RequiresRedis {

  import ExecutionContext.Implicits.global
  implicit val jedisPool = newJedisPool

  "A queue" should {
    "deliver messages to the listener" in new fixture {
      val latch = new CountDownLatch(1)
      var received: String = _

      override def listener = messageId => future { received = messageId; latch.countDown() }

      subject.enqueue("messageId")

      latch.await(1, TimeUnit.SECONDS) aka "message received" must beTrue
      received must_== "messageId"
    }

    "notify listener when processing starts" in new fixture {
      val latch = new CountDownLatch(1)
      var processingMessageId: String = _

      override def onProcessingStarted = messageId => { processingMessageId = messageId; latch.countDown() }

      subject.enqueue("messageId")

      latch.await(1, TimeUnit.SECONDS) aka "processing started" must beTrue
      processingMessageId must_== "messageId"
    }

    "notify listener when processing completes" in new fixture {
      val latch = new CountDownLatch(1)
      var completedMessageId: String = _

      override def onCompleted = messageId => { completedMessageId = messageId; latch.countDown() }

      subject.enqueue("messageId")

      latch.await(1, TimeUnit.SECONDS) aka "processing completed" must beTrue
      completedMessageId must_== "messageId"
    }

    "notify listener when processing times out" in new fixture {
      val latch = new CountDownLatch(1)
      var timedOutMessageId: String = _

      override def listener = _ => future { Thread.sleep(5000) }
      override def onTimedOut = messageId => { timedOutMessageId = messageId; latch.countDown() }

      subject.enqueue("messageId")

      latch.await(5, TimeUnit.SECONDS) aka "processing timed out" must beTrue
      timedOutMessageId must_== "messageId"
    }
  }

  step {
    jedisPool.destroy
  }

  trait fixture extends org.specs2.mutable.After with RedisFixture {
    implicit val actorSystem = ActorSystem("test-" + java.util.UUID.randomUUID())

    def listener: String => Future[Unit] = _ => Future { () }
    def onProcessingStarted: String => Unit = _ => ()
    def onCompleted: String => Unit = _ => ()
    def onTimedOut: String => Unit = _ => ()

    val subject: RedisQueue = new RedisQueue(prefix, Duration(2, TimeUnit.SECONDS))(listener)
    subject.monitoring.subscribe(actorSystem.actorOf(Props(new Actor {
      def receive = {
        case ProcessingStarted(messageId, _)   => onProcessingStarted(messageId)
        case ProcessingCompleted(messageId, _) => onCompleted(messageId)
        case ProcessingTimedOut(messageId, _)  => onTimedOut(messageId)
      }
    })))
    override def after = {
      subject.close()
      actorSystem.shutdown
      super.after
    }
  }
}
