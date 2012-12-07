package performance

import com.yammer.metrics.scala._
import events._
import eventstore._
import java.util.concurrent._
import models._
import controllers.PostsController
import controllers.routes
import org.scalacheck._
import org.apache.http._
import org.apache.http.client.HttpClient
import org.apache.http.client._
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods._
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager
import org.apache.http.impl.client._
import org.apache.http.message.BasicNameValuePair
import org.apache.http.protocol.HttpContext
import org.apache.http.util.EntityUtils
import play.api.libs.json._
import play.api.test._
import play.api.test.Helpers._
import play.api.libs.concurrent.Promise
import scala.collection.JavaConverters._
import scala.collection.parallel.immutable.ParVector
import eventstore.fake.FakeEventStore
import scala.util.Random

/*
 * Simple program to load test the blog posts server. Example command:
 *
 * SBT_OPTS="-XX:+UseParallelGC -XX:+TieredCompilation -Xms4G -Xmx4G" sbt 'test:run-main performance.PerformanceTest http://localhost:9000'
 *
 * Make sure you start with a clean blog posts server before running!
 */
object PerformanceTest extends App with Instrumented {
  val postContentForm = new PostsController(null).postContentForm

  val connMgr = new ThreadSafeClientConnManager
  connMgr.setDefaultMaxPerRoute(100)
  connMgr.setMaxTotal(100)

  val httpWithRedirect = new DefaultHttpClient(connMgr)
  val httpWithoutRedirect = new DefaultHttpClient(connMgr)
  httpWithoutRedirect.setRedirectStrategy(new DefaultRedirectStrategy {
    override def isRedirected(request: HttpRequest, response: HttpResponse, context: HttpContext) = false
  })
  implicit val http = httpWithRedirect

  val (hosts, concurrency, iterations) = args match {
    case Array(hosts, concurrency, iterations) => (hosts.split(","), concurrency.toInt, iterations.toInt)
    case _ =>
      println("Please specify the base URLs of the server (eg: http://localhost:9000) as the first parameter (comma separated), concurrency level, and total number of iterations")
      sys.exit(1)
  }

  val random = new scala.util.Random(42)
  def generatePosts(n: Int) = Vector.fill(n) {
    def randomAsciiString(min: Int, max: Int) = {
      new String(Array.fill(random.nextInt(max - min) + min)(random.nextPrintableChar))
    }
    val id = PostId.generate
    val title = randomAsciiString(10, 90)
    val content = randomAsciiString(250, 600)
    (id -> PostContent(title, content))
  }

  println("%-10s: %8s, %8s, %8s, %8s, %8s, %8s, %8s, %8s, %8s, %8s".
    format("task", "req/s", "min (ms)", "avg (ms)", "50% (ms)", "75% (ms)", "95% (ms)", "98% (ms)", "99% (ms)", "99.9% ms", "max (ms)"))

  def printTimer(name: String, timer: Timer) {
    val ss = timer.snapshot
    println("%-10s: %8.1f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f".
      format(name, timer.meanRate, timer.min, timer.mean, ss.getMedian, ss.get75thPercentile, ss.get95thPercentile, ss.get98thPercentile, ss.get99thPercentile, ss.get999thPercentile, timer.max))
  }

  def withTimer(name: String)(f: Timer => Unit) {
    val timer = metrics.timer(name)
    f(timer)
    printTimer(name, timer)
  }

  def execute(request: HttpUriRequest)(implicit httpClient: HttpClient) {
    val response = httpClient.execute(request)
    EntityUtils.consume(response.getEntity)
    if (response.getStatusLine.getStatusCode >= 400) {
      println("Bad request: " + response.getStatusLine)
    }
  }

  def contentPostParameters(request: HttpPost, content: PostContent) {
    val (fields, _) = postContentForm.mapping.unbind(content)
    val parameters = fields.map { field => new BasicNameValuePair(field._1, field._2) }.toList.asJava
    request.setEntity(new UrlEncodedFormEntity(parameters))
  }

  def randomHost(implicit random: Random) = hosts(random.nextInt(hosts.length))

  def showIndex(implicit timer: Timer, random: Random) = timer.time {
    execute(new HttpGet(randomHost + routes.PostsController.index.url))
  }

  def addPost(postId: PostId, content: PostContent)(implicit timer: Timer, random: Random) = timer.time {
    val request = new HttpPost(randomHost + routes.PostsController.add.submit(postId))
    contentPostParameters(request, content)
    execute(request)(httpWithoutRedirect)
  }

  def readPost(postId: PostId)(implicit timer: Timer, random: Random) = timer.time {
    execute(new HttpGet(randomHost + routes.PostsController.show(postId)))
  }

  def editPost(postId: PostId, content: PostContent)(implicit timer: Timer, random: Random) = timer.time {
    execute(new HttpGet(randomHost + routes.PostsController.show(postId)))
    val request = new HttpPost(randomHost + routes.PostsController.edit.submit(postId, StreamRevision(1)))
    contentPostParameters(request, content)
    execute(request)(httpWithRedirect)
  }

  def deletePost(postId: PostId)(implicit timer: Timer, random: Random) = timer.time {
    val request = new HttpPost(randomHost + routes.PostsController.delete(postId, StreamRevision(2)))
    execute(request)(httpWithoutRedirect)
  }

  def runParallel[A](n: Int, data: Seq[A])(f: Seq[A] => Unit) {
    val executor = Executors.newFixedThreadPool(n)
    val split = data.grouped(data.size / n).toIndexedSeq
    for (i <- 0 until n) {
      executor.execute(new Runnable {
        override def run {
          try f(split(i)) catch {
            case e: Exception =>
              Console.err.println("Error: " + e)
              e.printStackTrace
          }
        }
      })
    }
    executor.shutdown
    if (!executor.awaitTermination(1, TimeUnit.HOURS)) {
      println("Timeout awaiting shutdown of executor.")
      executor.shutdownNow
    }
  }

  {
    val Concurrency = 4 * hosts.length
    val Iterations = 5000
    val Total = Concurrency * Iterations
    val posts = generatePosts(Total)
    withTimer("warm-up") { implicit timer =>
      runParallel(Concurrency, posts) { posts =>
        implicit val random = new Random()
        for ((id, content) <- posts) {
          addPost(id, content)
          showIndex
          readPost(id)
          editPost(id, content)
          deletePost(id)
        }
      }
    }
  }

  {
    val Concurrency = concurrency
    val Total = iterations
    val Iterations = Total / Concurrency
    val posts = generatePosts(Total)
    withTimer("add posts") { implicit timer =>
      runParallel(Concurrency, posts) { posts =>
        implicit val random = new Random()
        for ((id, content) <- posts) addPost(id, content)
      }
    }
    withTimer("read posts") { implicit timer =>
      runParallel(Concurrency, posts) { posts =>
        implicit val random = new Random()
        for ((id, content) <- posts) readPost(id)
      }
    }
    withTimer("edit posts") { implicit timer =>
      runParallel(Concurrency, posts) { posts =>
        implicit val random = new Random()
        for ((id, content) <- posts) editPost(id, content.copy(body = content.body.reverse))
      }
    }
    withTimer("list posts") { implicit timer =>
      runParallel(Concurrency, posts.take(posts.size / 4)) { posts =>
        implicit val random = new Random()
        for ((id, content) <- posts) showIndex
      }
    }
  }


  connMgr.shutdown
  println("Done.")
  System.exit(0)
}
