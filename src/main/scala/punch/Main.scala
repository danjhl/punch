package punch

import punch.cli.Cli

object Main extends App {
  override def main(args: Array[String]): Unit = {
    Cli.interpret(args).unsafeRunSync()
  }
}
