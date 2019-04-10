package punch.cli

import org.jline.terminal.TerminalBuilder
import org.jline.reader.LineReaderBuilder
import cats.effect.IO

object Repl {
  def start(projectName: String): IO[Unit] = {
    IO {  
      val terminal = TerminalBuilder.terminal();
      val reader = LineReaderBuilder.builder().terminal(terminal).build()
      val prompt = s"${Console.BLUE}punch> "

      // TODO try to use foreverM
      // catch exceptions eg. ctrl + c
      while (true) {
          val line = reader.readLine(prompt)
          val cmd = Parser.parseLine(line)
          cmd.map(eval)
      }
    }
  }

  // TODO return error
  def eval(cmd: ReplCommand): IO[Unit] = {
    IO {
      cmd match {
        case Ls => println("ls")
        // TODO handle all remove _
        case _ => println("unknown command")
      }
    }
  }

  // to persistenc
}