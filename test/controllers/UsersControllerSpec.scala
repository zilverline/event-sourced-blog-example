package controllers

import play.api.test._
import play.api.test.Helpers._
import events._
import eventstore._
import models._
import org.specs2.mutable.After
import scala.collection.immutable.SortedMap

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class UsersControllerSpec extends org.specs2.mutable.Specification {
  val userId = UserId.generate()

  "users controller" should {
    "register a new user" in new fixture {
      val request = FakeRequest().withFormUrlEncodedBody("email" -> "john@example.com", "password.1" -> "password", "password.2" -> "password")

      val result = subject.register.submit(request)

      status(result) must_== 303
      users.get(Email("john@example.com")) map { user =>
        user.login must_== Email("john@example.com")
        user.password.verify("password") aka "password verified" must beTrue
      } getOrElse {
        failure("user not registered")
      }
    }

  }

  trait fixture extends After { self =>
    val eventStore: EventStore[UserEvent] = new fake.FakeEventStore

    val memoryImage = MemoryImage[State, UserEvent](eventStore)(State()) {
      (state, commit) => state.updateMany(commit.eventsWithRevision)
    }

    val subject = new UsersController(memoryImage)

    def users = memoryImage.get.users

    override def after {
      eventStore.close
    }
  }
}
