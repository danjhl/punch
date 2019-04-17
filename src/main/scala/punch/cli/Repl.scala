package punch.cli

import scala.util.{Success, Failure, Try}
import cats.effect.IO
import cats.effect.concurrent.MVar
import org.jline.terminal.TerminalBuilder
import org.jline.reader.LineReaderBuilder
import java.time.Instant

object Repl {
  // TODO remove
  var state: Option[(String, Long)] = None

  def start(project: String): IO[Unit] = {
    IO {  
      val terminal = TerminalBuilder.terminal()
      val reader = LineReaderBuilder.builder().terminal(terminal).build()
      val prompt = s"${Console.BLUE}punch> "

      // TODO try to use foreverM
      // or recursive?
      // MVar ?
      // catch exceptions eg. ctrl + c
      while (true) {
          val line = reader.readLine(prompt)
          val cmd = Parser.parseLine(line)

          // remove unsafeRun
          cmd.map(eval(_, project)).map(_.unsafeRunSync())
      }
    }
  }

  // TODO return error
  // to persistenc
  private def eval(cmd: ReplCommand, project: String): IO[Unit] = {
    // TODO handle all remove _

    cmd match {
      case Ls            => Persistence.readActivities().flatMap(print)
      case Now(activity) => now(project, activity)
      case Stop          => stop(project)
      case _             => IO { println("unknown command") }
    }
  }

  private def now(project: String, activity: String) = {
    IO {
      println(s"tracking ${project}/${activity}")
      state = Some((activity, Instant.now().getEpochSecond)) 
    }
  }

  private def stop(project: String) = {
    state match {
      case Some(t) => store(t, project).flatMap(_ => IO { state = None })
      case None    => IO { println("not tracking") }
    }
  }

  private def store(t: (String, Long), projectName: String) = {
    val act = Activity(t._1, projectName, t._2, Instant.now().getEpochSecond)
    Persistence.writeActivity(act).map(result => result match {
      case Success(_)   => {}
      case Failure(err) => scribe.error(err.getMessage)
    })
  }

  private def print(result: Try[Seq[Activity]]): IO[Unit] = {
    IO {
      result match {
        case Success(seq) =>
          println("activities: \n")
          println(DisplayText.listSums(seq))
        case Failure(t) => 
          scribe.error(t.getMessage())
      }
    }
  }
}