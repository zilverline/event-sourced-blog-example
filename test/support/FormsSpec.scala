package support

import Forms._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class FormsSpec extends org.specs2.mutable.Specification {

  "Tokenized text" should {
    "remove trailing space" in {
      tokenizedText.bind(Map("" -> "Joe ")) must beRight("Joe")
    }

    "remove leading space" in {
      tokenizedText.bind(Map("" -> " Joe")) must beRight("Joe")
    }

    "collapse multiple spaces" in {
      tokenizedText.bind(Map("" -> "Joe   \t Doe")) must beRight("Joe Doe")
    }
  }
}