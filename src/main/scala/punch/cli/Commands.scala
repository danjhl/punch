package punch.cli

object Commands {
  def parse(args: Seq[String]): Either[InterpretError, Command] = args match {
    case Seq(Patterns.help(_)) => Right(HelpCmd)
    case Seq(projectName)      => Right(SwitchCmd(projectName))
    case _                     => Left(UnknownCommand)
  }
}

private object Patterns {
  val help = """^(help)$""".r
}

sealed trait Command
object HelpCmd extends Command
case class SwitchCmd(projectName: String) extends Command

sealed trait InterpretError
final case object UnknownCommand extends InterpretError