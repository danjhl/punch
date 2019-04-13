package punch.cli

import cats.effect.IO

object Cli {
  def interpret(args: Seq[String]): IO[Unit] = {
    Commands.interpret(args) match {
      case Right(ShowHelp)            => Help.show()
      case Right(Switch(projectName)) => Repl.start(projectName)
      case Left(UnknownCommand)       => unknown(args)
    }
  }

  private def unknown(args: Seq[String]) = args match {
    case head :: _ => printError(s"unknown command ${head}")
    case Nil       => printError(s"no command parsed for ${args.mkString}")
  }

  private def printError(message: String): IO[Unit] = {
    IO { println(s"error: $message")}
  }
}