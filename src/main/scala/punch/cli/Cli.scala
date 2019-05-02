package punch.cli

import punch.cli.DisplayText._
import scalaz.zio.{IO, Task}
import scala.util.Failure
import scala.util.Success

object Cli {
  val repo = Repo

  def interpret(args: Seq[String]): Task[Unit] = {
    Commands.interpret(args) match {
      case Right(ShowHelp())         => Help.show()
      case Right(Switch(project))    => Repl.start(project)
      case Right(RmProject(project)) => rm(project)
      case Left(UnknownCommand())    => unknown(args)
    }
  }

  private def rm(project: String) = {
    repo.deleteProject(project)
  }

  private def unknown(args: Seq[String]) = args match {
    case head :: _ => putStrLn(s"unknown command $head")
    case Nil       => putLogErr(s"no command parsed for $args")
  }
}