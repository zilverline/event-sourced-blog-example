import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName = "play-blog"
  val appVersion = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    "redis.clients" % "jedis" % "2.1.0",
    "nl.grons" %% "metrics-scala" % "2.2.0",
    "com.lambdaworks" % "scrypt" % "1.3.3",
    "org.apache.httpcomponents" % "httpcore" % "4.1.2",
    "org.apache.httpcomponents" % "httpclient" % "4.1.2",
    "org.specs2" %% "specs2" % "1.12.3" % "test",
    "org.scalacheck" %% "scalacheck" % "1.10.0" % "test")

  val main = play.Project(appName, appVersion, appDependencies).settings(
    scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-Ywarn-value-discard", "-Ywarn-adapted-args"),

    // Avoid running tests using both specs2 and junit runner.
    testFrameworks in Test := Seq(TestFrameworks.Specs2),

    // Override Play! defaults to enable parallel test execution
    testOptions in Test := Seq(Tests.Argument(TestFrameworks.Specs2, "junitxml", "console")),

    routesImport ++= Seq("events._", "eventstore.{ StoreRevision, StreamRevision }", "support.Binders._"),

    templatesImport ++= Seq("events._", "eventstore.{ StoreRevision, StreamRevision }"),

    lessEntryPoints <<= (sourceDirectory in Compile)(base => (
      (base / "assets" / "stylesheets" / "bootstrap" / "bootstrap.less") +++
      (base / "assets" / "stylesheets" / "bootstrap" / "responsive.less"))))

}
