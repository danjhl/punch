package punch

import punch.cli.Cli
import punch.cli.DisplayText._
import scalaz.zio.{App, ZIO}

object Main extends App {
  override def run(args: List[String]): ZIO[Environment, Nothing, Int] = {
    Cli
      .interpret(args)
      .catchAll(err => putLogErr(err.getMessage()))
      .fold(_ => 1, _ => 0)
  }
}
