package punch.cli

import punch.io.ConsoleImpl.putStrLn
import punch.io.RepositoryImpl
import punch.repl.Repl
import zio.{IO, Task}
import scala.util.Failure
import scala.util.Success

object Cli {
  val repo = RepositoryImpl

  def interpret(args: Seq[String]): Task[Unit] = {
    CliParser.parse(args) match {
      case Right(LsProjects())       => ls()
      case Right(ShowHelp())         => Help.show()
      case Right(Switch(project))    => Repl.start(project)
      case Right(RmProject(project)) => rm(project)
      case Left(UnknownCliCommand()) => unknown(args)
    }
  }

  private def ls() = {
    repo.readProjects().flatMap(projects => putStrLn(projects.mkString("\n")))
  }

  private def rm(project: String) = {
    repo.deleteProject(project)
  }

  private def unknown(args: Seq[String]) = args match {
    case head :: _ => putStrLn(s"unknown command '$head'")
    case Nil       => IO { scribe.error(s"no command parsed for '$args'") }
  }
}