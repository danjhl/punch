import sbt._

object Deps {
  lazy val scalaTest  = "org.scalatest" %% "scalatest"   % "3.0.5"
  lazy val scribe     = "com.outr"      %% "scribe"      % "2.7.3"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "1.2.0"
  lazy val jline      = "org.jline"     %  "jline"       % "3.1.3"
  lazy val fastparse  = "com.lihaoyi"   %% "fastparse"   % "2.1.0"
}
