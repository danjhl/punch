package punch.cli

import org.jline.terminal.TerminalBuilder
import org.jline.reader.LineReaderBuilder
import cats.effect.IO

object Repl {
  def start(projectName: String): IO[Unit] = {
    IO {  
      val terminal = TerminalBuilder.terminal()
      val reader = LineReaderBuilder.builder().terminal(terminal).build()
      val prompt = s"${Console.BLUE}punch> "

      // TODO try to use foreverM
      // catch exceptions eg. ctrl + c
      while (true) {
          val line = reader.readLine(prompt)
          val cmd = Parser.parseLine(line)

          // remove unsafeRun
          cmd.map(eval).map(_.unsafeRunSync())
      }
    }
  }

  // TODO return error
  // to persistenc
  private def eval(cmd: ReplCommand): IO[Unit] = {
    cmd match {
      case Ls => ls()
      case Now(activityName) => IO { println(s"tracking $activityName") }
      // TODO handle all remove _
      case _ => IO { println("unknown command") }
    }
  }

  private def ls(): IO[Unit] = {
    // todo use jline Console write
    for {
      result <- Persistence.readActivities()
      _      <- IO { println("activities: \n") }
      // todo handle error
      _      <- IO { result.map(x => println(PrettyText.pretty(x))) }
    } yield ()
  }

}