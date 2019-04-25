package punch.cli

import scala.util.{Success, Failure, Try}
import cats.effect.IO
import org.jline.terminal.{TerminalBuilder, Terminal}
import org.jline.reader.{LineReaderBuilder, Candidate}
import org.jline.reader.impl.completer.StringsCompleter
import java.time.{LocalDate, Instant, ZoneId}

object Repl {
  val store = Persistence
  // TODO remove
  var state: Option[(String, Long)] = None

  def start(project: String): IO[Unit] = {
    IO {  
      val terminal = TerminalBuilder.terminal()

      val reader = LineReaderBuilder.builder()
        .completer((reader, line, candidates) => {
          if (line.line.matches("((now)\\s*?.*)|((rm)\\s*?.*)")) {
            store.readActivities()
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
      case Ls(para)      => ls(para, project)
      case Now(activity) => now(project, activity)
      case Stop          => stop(project)
      case Exit          => exit(project)
      case Rm(activity)  => rm(activity, project)
      case _             => IO { println("unknown command") }
    }
  }

  // TODO only list current project
  private def ls(para: Option[TimePara], project: String) = para match {
    case None       => store.readActivitiesFor(project).flatMap(print)
    case Some(Day)  => lsWith(Activity.onDay _, project)
    case Some(Week) => lsWith(Activity.inWeek _, project)
  }

  private def lsWith(fn: (Long, LocalDate, ZoneId) => Boolean, project: String) = {
    val filter = fn(_, LocalDate.now(), ZoneId.systemDefault())

    for {
      result   <- store.readActivitiesFor(project)
      filtered <- IO { result.map(seq => seq.filter(a => filter(a.from))) }
      effect   <- print(filtered)
    } yield effect 
  }

  private def now(project: String, activity: String) = IO {
    println(s"tracking ${project}/${activity}")
    state = Some((activity, Instant.now().getEpochSecond)) 
  }

  private def stop(project: String) = state match {
    case Some((activity, time)) => save(project, activity, time)
    case None                   => IO { println("not tracking") }
  }

  private def save(project: String, activity: String, from: Long) = {
    val to = Instant.now().getEpochSecond
    val act = Activity(activity, project, from, to)
    
    store.writeActivity(act).map {
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

  private def rm(activity: String, project: String) = {
    store.deleteActivities(activity, project).map {
      case Success(_)   => {}
      case Failure(err) => scribe.error(err.getMessage())
    }
  }
}