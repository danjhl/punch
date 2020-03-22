package punch

import punch.cli.Cli
import zio.{App, ZIO, IO}

object Main extends App {
  override def run(args: List[String]) = {
    Cli
      .interpret(args)
      .catchAll(err => IO { scribe.error(err) })
      .fold(_ => 1, _ => 0)
  }
}
