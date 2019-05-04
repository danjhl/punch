package punch.cli

import punch.cli.DisplayText._
import scalaz.zio.{IO, Task}
import scala.util.Failure
import scala.util.Success

object Cli {
  val repo = Repo

  def interpret(args: Seq[String]): Task[Unit] = {
    Commands.interpret(args) match {
      case Right(LsProjects())       => ls()
      case Right(ShowHelp())         => Help.show()
      case Right(Switch(project))    => Repl.start(project)
      case Right(RmProject(project)) => rm(project)
      case Left(UnknownCommand())    => unknown(args)
    }
  }

  private def ls() = {
    repo.readProjects().flatMap(projects => putStrLn(projects.mkString("\n")))
  }

  private def rm(project: String) = {
    repo.deleteProject(project)
  }

  private def unknown(args: Seq[String]) = args match {
    case head :: _ => putStrLn(s"unknown command $head")
    case Nil       => IO { scribe.error(s"no command parsed for $args") }
  }
}