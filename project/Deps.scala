import sbt._

object Deps {
  lazy val scalaTest   = "org.scalatest"        %% "scalatest"            % "3.0.5"
  lazy val scribe      = "com.outr"             %% "scribe"               % "2.7.3"
  lazy val zio         = "org.scalaz"           %% "scalaz-zio"           % "1.0-RC4"
  lazy val fastparse   = "com.lihaoyi"          %% "fastparse"            % "2.1.0"
  lazy val jlineNative = "org.jline"            %  "jline"                % "3.1.3"
  lazy val jline       = "org.jline"            %  "jline"                % "3.14.0"
  lazy val jlineJansi  = "org.jline"            %  "jline-terminal-jansi" % "3.14.0"
  lazy val jansi       = "org.fusesource.jansi" %  "jansi"                % "1.18"
}
