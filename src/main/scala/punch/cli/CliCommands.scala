package punch.cli

object CliCommands {
  def interpret(args: Seq[String]): Either[CommandError, Command] = args match {
    case Nil              => Right(ShowHelp())
    case Seq(ls(_))       => Right(LsProjects())
    case Seq(help(_))     => Right(ShowHelp())
    case Seq(rm(_), name) => Right(RmProject(name))
    case Seq(projectName) => Right(Switch(projectName))
    case _                => Left(UnknownCommand())
  }

  private val ls = """^(ls)$""".r
  private val help = """^(help)$""".r
  private val rm = """^(rm)$""".r
}

sealed trait Command

case class LsProjects() extends Command
case class ShowHelp() extends Command
case class Switch(projectName: String) extends Command
case class RmProject(projectName: String) extends Command

sealed trait CommandError

case class UnknownCommand() extends CommandError