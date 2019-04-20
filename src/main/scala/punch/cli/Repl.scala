package punch.cli

import scala.util.{Success, Failure, Try}
import cats.effect.IO
import org.jline.terminal.{TerminalBuilder, Terminal}
import org.jline.reader.{LineReaderBuilder, Candidate}
import org.jline.reader.impl.completer.StringsCompleter
import java.time.{LocalDate, Instant, ZoneId}

object Repl {
  // TODO remove
  var state: Option[(String, Long)] = None

  def start(project: String): IO[Unit] = {
    IO {  
      val terminal = TerminalBuilder.builder()
        .signalHandler(signal => {
          println(signal)
          if (signal == Terminal.Signal.QUIT) {exit(project)}
        })
        .build()

      val reader = LineReaderBuilder.builder()
        .completer((reader, line, candidates) => {
          if (line.line.matches("(now)\\s*?.*")) {
            Persistence.readActivities()
              .map(t => t.map(s => s.map(_.name)
                .toSeq
                .foreach(name => candidates.add(new Candidate(name)))))
              .unsafeRunSync()
          }
        })
        .terminal(terminal)
        .build()
      val prompt = s"${Console.BLUE}punch> "

      // TODO try to use foreverM
      // or recursive?import org.jline.terminal.TerminalBuilder
      // MVar ?
      // catch exceptions eg. ctrl + c
      while (true) {
          import org.jline.reader.UserInterruptException
          Try(reader.readLine(prompt)) match {
            case Success(line) => 
              val cmd = Parser.parseLine(line)
              cmd.map(eval(_, project)).map(_.unsafeRunSync())
            case Failure(err: UserInterruptException) =>
              if (state.isDefined) stop(project).unsafeRunSync()
              System.exit(0)
            case Failure(err) =>
              scribe.error(err.getMessage())
              if (state.isDefined) stop(project).unsafeRunSync()
          }
      }
    }
  }

  // TODO return error
  private def eval(cmd: ReplCommand, project: String): IO[Unit] = {
    // TODO handle all remove _

    cmd match {
      case Ls(para)      => ls(para)
      case Now(activity) => now(project, activity)
      case Stop          => stop(project)
      case Exit          => exit(project)
      case Rm(activity)  => Persistence.deleteActivities(activity).map(_ => {})
      case _             => IO { println("unknown command") }
    }
  }

  private def ls(para: Option[TimePara]) = para match {
    case None       => Persistence.readActivities().flatMap(print)
    case Some(Day)  => lsWith(Activity.onDay _)
    case Some(Week) => lsWith(Activity.inWeek _)
  }

  private def lsWith(fn: (Long, LocalDate, ZoneId) => Boolean) = {
    val filter = fn(_, LocalDate.now(), ZoneId.systemDefault())

    for {
      result   <- Persistence.readActivities()
      filtered <- IO { result.map(seq => seq.filter(a => filter(a.seconds))) }
      effect   <- IO[Unit] { print(filtered) }
    } yield effect 
  }

  private def now(project: String, activity: String) = IO {
    println(s"tracking ${project}/${activity}")
    state = Some((activity, Instant.now().getEpochSecond)) 
  }

  private def stop(project: String) = state match {
    case Some((activity, time)) => store(project, activity, time)
    case None                   => IO { println("not tracking") }
  }

  private def store(project: String, activity: String, from: Long) = {
    val to = Instant.now().getEpochSecond
    val act = Activity(activity, project, from, to)
    
    Persistence.writeActivity(act).map {
      case Success(_)   => state = None
      case Failure(err) => scribe.error(err.getMessage)
    }
  }

  private def print(result: Try[Seq[Activity]]): IO[Unit] = result match {
    case Success(seq) =>
      IO { 
        println("activities: \n")
        println(DisplayText.listSums(seq)) 
      }
    case Failure(t) => 
      IO { scribe.error(t.getMessage()) }
  }

  private def exit(project: String) = {
    if (state.isDefined) 
      stop(project).flatMap(x => IO { System.exit(0) })
    else 
      IO { System.exit(0) }
  }
}