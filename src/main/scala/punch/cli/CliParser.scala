package punch.cli

object CliParser {
  def parse(args: Seq[String]): Either[CliCommandError, CliCommand] = 
    args match {
      case Nil              => Right(ShowHelp())
      case Seq(ls(_))       => Right(LsProjects())
      case Seq(help(_))     => Right(ShowHelp())
      case Seq(rm(_), name) => Right(RmProject(name))
      case Seq(projectName) => Right(Switch(projectName))
      case _                => Left(UnknownCliCommand())
    }

  private val ls = """^(ls)$""".r
  private val help = """^(help)$""".r
  private val rm = """^(rm)$""".r
}

sealed trait CliCommand

case class LsProjects() extends CliCommand
case class ShowHelp() extends CliCommand
case class Switch(projectName: String) extends CliCommand
case class RmProject(projectName: String) extends CliCommand

sealed trait CliCommandError

case class UnknownCliCommand() extends CliCommandError