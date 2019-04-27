package punch

import punch.cli.Cli
import cats.effect.{ExitCode, IOApp, IO}

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    Cli.interpret(args)
  }
}
