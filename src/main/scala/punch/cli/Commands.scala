package punch.cli

object Commands {
  def interpret(args: Seq[String]): Either[CommandError, Command] = args match {
    case Seq(Patterns.help(_)) => Right(ShowHelp)
    case Seq(projectName)      => Right(Switch(projectName))
    case _                     => Left(UnknownCommand)
  }
}

private object Patterns {
  val help = """^(help)$""".r
}

sealed trait Command
object ShowHelp extends Command
case class Switch(projectName: String) extends Command

sealed trait CommandError
final case object UnknownCommand extends CommandError
