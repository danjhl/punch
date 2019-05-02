package punch.cli

import punch.cli.DisplayText._
import scala.util.{Success, Failure, Try}
import scalaz.zio.{IO, Task}
import org.jline.terminal.{TerminalBuilder, Terminal}
import org.jline.reader.{LineReaderBuilder, LineReader, Candidate}
import org.jline.reader.impl.completer.StringsCompleter
import org.jline.reader.UserInterruptException
import java.time.{LocalDate, Instant, ZoneId}

case class State(tracked: Option[(String, Long)], project: String)

object Repl {
  val repo = Repo
  val prompt = s"${Console.BLUE}punch> "

  def start(project: String): Task[Unit] = {
    for {
      terminal <- IO { TerminalBuilder.terminal() }
      _        <- loop(State(None, project), terminal)
    } yield ()
  }

  def loop(state: State, terminal: Terminal): Task[Unit] = {
    for {
      reader <- createReader(terminal)
      _      <- readInput(reader, state, terminal)
    } yield ()
  }

  def createReader(terminal: Terminal): Task[LineReader] = {
    for {
      activities <- repo.readActivities()
      reader     <- IO { 
        LineReaderBuilder.builder()
          .completer { (reader, line, candidates) =>
            if (activityCmd(line.line))
              activities.foreach(a => candidates.add(new Candidate(a.name)))
          }
          .terminal(terminal)
          .build()
      }
    } yield reader
  }

  def activityCmd(line: String) = line.matches("((now)\\s*?.*)|((rm)\\s*?.*)")

  def readInput(
      reader: LineReader, 
      state: State,
      terminal: Terminal): Task[Unit] = {

    for {
      line <- IO { reader.readLine(prompt) }
      _    <- handleInput(line, state, terminal).catchSome {
        case _ : UserInterruptException => handleInterrupt(state)
      }
    } yield ()
  }

  def handleInput(
      line: String, 
      state: State,
      terminal: Terminal): Task[Unit] = {

    Parser.parseLine(line) match {
      case Left(err)  => putStrLn(err.message)
      case Right(cmd) => cmd match {

        case Ls(para) => 
          ls(para, state.project).flatMap(_ => loop(state, terminal))

        case Now(activity) =>
          now(activity, state, terminal)

        case Stop if state.tracked.isDefined => 
          state.tracked
            .map { tracked =>
              stop(state.project, tracked._1, tracked._2).flatMap { _ => 
                loop(state.copy(tracked = None), terminal)
              }
            }
            .getOrElse {
              putStrLn("not tracking").flatMap(_ => loop(state, terminal))
            }

        
        case Exit => 
          state.tracked match {
            case None                   => IO {}
            case Some((activity, time)) => stop(state.project, activity, time)
          }

        case Rm(activity) => 
          repo
            .deleteActivities(activity, state.project)
            .flatMap(_ => loop(state, terminal))

        case _ => 
          putStrLn("unknown command").flatMap(_ => loop(state, terminal))
      }
    }
  }

  def handleInterrupt(state: State) = state match {
      case State(Some((name, time)), project) => stop(project, name, time)
      case State(None, _)                     => IO {}
  }

  def stop(project: String, activity: String, time: Long): Task[Unit] = {
    for {
      end <- IO { Instant.now() }
      act <- IO { Activity(activity, project, time, end.getEpochSecond) }
      _   <- repo.writeActivity(act)
    } yield ()
  }

  def now(activity: String, state: State, terminal: Terminal) = {
    for {
      _      <- state match {
        case State(Some((name, time)), project) => stop(project, name, time)
        case _                                  => IO {}
      }
      _      <- putStrLn(s"tracking ${state.project}/${activity}")
      time   <- IO { Instant.now() }
      result <- loop(state.copy(
        tracked = Some((activity, time.getEpochSecond()))),
        terminal)
    } yield result
  }

  private def ls(para: Option[TimePara], project: String) = para match {
    case None       => repo.readActivitiesFor(project).flatMap(printAct)
    case Some(Day)  => lsWith(Activity.onDay _, project)
    case Some(Week) => lsWith(Activity.inWeek _, project)
  }

  private def lsWith(
      fn: (Long, LocalDate, ZoneId) => Boolean,
      project: String) = {
    
    for {
      date   <- IO { LocalDate.now() }
      zoneId <- IO { ZoneId.systemDefault() }
      seq    <- 
        repo
          .readActivitiesFor(project)
          .map(_.filter(a => fn(a.from, date, zoneId)))

      effect <- printAct(seq)
    } yield effect 
  }

  private def printAct(seq: Seq[Activity]) = {
    putStrLn("activities: \n\n" + DisplayText.listSums(seq) + "\n")
  }
}