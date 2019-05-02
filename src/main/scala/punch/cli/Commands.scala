package punch.cli

object Commands {
  def interpret(args: Seq[String]): Either[CommandError, Command] = args match {
    case Nil                       => Right(ShowHelp())
    case Seq(Patterns.help(_))     => Right(ShowHelp())
    case Seq(Patterns.rm(_, name)) => Right(RmProject(name))
    case Seq(projectName)          => Right(Switch(projectName))
    case _                         => Left(UnknownCommand())
  }
}

private object Patterns {
  val help = """^(help)$""".r
  val rm = """^(rm) (.*)""".r
}

sealed trait Command
case class ShowHelp() extends Command
case class Switch(projectName: String) extends Command
case class RmProject(projectName: String) extends Command

sealed trait CommandError
case class UnknownCommand() extends CommandError