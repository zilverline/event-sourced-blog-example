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

/*
 * Simple program to load test the blog posts server. Example command:
 *
 * SBT_OPTS="-XX:+UseParallelGC -XX:+TieredCompilation -Xms4G -Xmx4G" sbt 'test:run-main performance.PerformanceTest http://localhost:9000'
 *
 * Make sure you start with a clean blog posts server before running!
 */
object PerformanceTest extends App with Instrumented {
  running(FakeApplication()) {} // Let play initialize the logger

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

  val host = args match {
    case Array(host) => host
    case _ =>
      println("Please specify the base URL of the server (eg: http://localhost:9000) as the first parameter")
      sys.exit(1)
  }

  val random = new scala.util.Random(42)
  def generatePosts(n: Int) = ParVector.fill(n) {
    def randomAsciiString(min: Int, max: Int) = {
      new String(Array.fill(random.nextInt(max - min) + min)(random.nextPrintableChar))
    }
    val id = PostId.generate
    val author = randomAsciiString(10, 40)
    val title = randomAsciiString(10, 90)
    val content = randomAsciiString(250, 600)
    (id -> PostContent(author, title, content))
  }.seq

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

  def showIndex(implicit timer: Timer) = timer.time {
    execute(new HttpGet(host + routes.PostsController.index.url))
  }

  def addPost(postId: PostId, content: PostContent)(implicit timer: Timer) = timer.time {
    val request = new HttpPost(host + routes.PostsController.add.submit(postId))
    contentPostParameters(request, content)
    execute(request)(httpWithoutRedirect)
  }

  def readPost(postId: PostId)(implicit timer: Timer) = timer.time {
    execute(new HttpGet(host + routes.PostsController.show(postId)))
  }

  def editPost(postId: PostId, content: PostContent)(implicit timer: Timer) = timer.time {
    val request = new HttpPost(host + routes.PostsController.edit.submit(postId, StreamRevision(1)))
    contentPostParameters(request, content)
    execute(request)(httpWithRedirect)
  }

  def deletePost(postId: PostId)(implicit timer: Timer) = timer.time {
    val request = new HttpPost(host + routes.PostsController.delete(postId, StreamRevision(2)))
    execute(request)(httpWithoutRedirect)
  }

  def runParallel[A](n: Int, data: Seq[A])(f: Seq[A] => Unit) {
    val executor = Executors.newFixedThreadPool(n)
    val split = data.grouped(data.size / n).toIndexedSeq
    for (i <- 0 until n) {
      executor.execute(new Runnable {
        override def run {
          f(split(i))
        }
      })
    }
    executor.shutdown
    executor.awaitTermination(5, TimeUnit.MINUTES)
  }

  {
    val Concurrency = 4
    val Total = 10000
    val Iterations = Total / Concurrency
    val posts = generatePosts(Total)
    withTimer("warm-up") { implicit timer =>
      runParallel(Concurrency, posts) { posts =>
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
    val Concurrency = 50
    val Total = 100 * 1000
    val Iterations = Total / Concurrency
    val posts = generatePosts(Total)
    withTimer("add posts") { implicit timer =>
      runParallel(Concurrency, posts) { posts =>
        for ((id, content) <- posts) addPost(id, content)
      }
    }
    withTimer("read posts") { implicit timer =>
      runParallel(Concurrency, posts) { posts =>
        for ((id, content) <- posts) readPost(id)
      }
    }
    withTimer("edit posts") { implicit timer =>
      runParallel(Concurrency, posts) { posts =>
        for ((id, content) <- posts) editPost(id, content.copy(body = content.body.reverse))
      }
    }
    withTimer("list posts") { implicit timer =>
      runParallel(Concurrency, posts) { posts =>
        for ((id, content) <- posts) showIndex
      }
    }
  }

  println("Done.")
  System.exit(0)
}
