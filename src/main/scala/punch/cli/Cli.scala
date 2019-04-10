package punch.cli

import cats.effect.IO

object Cli {
  def interpret(args: Array[String]): IO[Unit] = {
    Commands.interpret(args) match {
      case Right(ShowHelp)               => Help.show()
      case Right(Switch(projectName))    => Repl.start(projectName)
      case Left(UnknownCommand)          => unknown(args)
    }
  }

  private def unknown(args: Array[String]) = {
    printError(s"unknown command ${args.head}")
  }

  private def printError(message: String): IO[Unit] = {
    IO { println(s"error: $message")}
  }
}