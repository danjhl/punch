package punch.cli

import cats.effect.{IO, ExitCode}
import scala.util.Failure
import scala.util.Success

object Cli {
  val store = Persistence

  def interpret(args: Seq[String]): IO[ExitCode] = {
    Commands.interpret(args) match {
      case Right(ShowHelp)           => Help.show().map(_ => ExitCode.Success)
      case Right(Switch(project))    => Repl.start(project)
      case Right(RmProject(project)) => rm(project)
      case Left(UnknownCommand)      => unknown(args).map(_ => ExitCode.Error)
    }
  }

  private def rm(project: String) = {
    store.deleteProject(project).map {
      case Success(value) => ExitCode.Success
      case Failure(err) => println(err.getMessage()); ExitCode.Error
    }
  }

  private def unknown(args: Seq[String]) = args match {
    case head :: _ => putStrLn(s"unknown command $head")
    case Nil       => putLogErr(s"no command parsed for $args")
  }

  def putStrLn(msg: String) = IO { println(msg) }
  def putLogErr(msg: String) = IO { scribe.error(msg) }
}