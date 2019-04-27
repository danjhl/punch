package punch.cli

import Cli.{putStrLn, putLogErr}
import scala.util.{Success, Failure, Try}
import cats.effect.{IO, ExitCode}
import org.jline.terminal.{TerminalBuilder, Terminal}
import org.jline.reader.{LineReaderBuilder, LineReader, Candidate}
import org.jline.reader.impl.completer.StringsCompleter
import org.jline.reader.UserInterruptException
import java.time.{LocalDate, Instant, ZoneId}
import cats.effect.ExitCode

case class State(tracked: Option[(String, Long)], project: String)

object Repl {
  val store = Persistence
  val prompt = s"${Console.BLUE}punch> "

  def start(project: String): IO[ExitCode] = {
    for {
      terminal <- IO { TerminalBuilder.terminal() }
      exitCode <- loop(State(None, project), terminal)
    } yield exitCode
  }

  def loop(state: State, terminal: Terminal): IO[ExitCode] = {
    for {
      result <- createReader(terminal)
      exitCode <- result match {
        case Failure(err) => putLogErr(err.getMessage).map(_ => ExitCode.Error)
        case Success(reader) => readInput(reader, state, terminal)
      }
    } yield exitCode
  }

  def createReader(terminal: Terminal): IO[Try[LineReader]] = {
    for {
      result     <- store.readActivities()
      activities <- result match {
        case Success(seq) => IO.pure(seq.toSet)
        case Failure(err) =>
          putLogErr(err.getMessage).flatMap(_ => IO.pure(Set.empty[Activity]))
      }
      reader <- IO { 
        Try {
          LineReaderBuilder.builder()
            .completer((reader, line, candidates) => {
              if (activityCmd(line.line))
                activities.foreach(a => candidates.add(new Candidate(a.name)))
            })
            .terminal(terminal)
            .build()
        }
      }
    } yield reader
  }

  def activityCmd(line: String) = line.matches("((now)\\s*?.*)|((rm)\\s*?.*)")

  def readInput(
      reader: LineReader, 
      state: State,
      terminal: Terminal): IO[ExitCode] = {

    for {
      result <- IO { Try(reader.readLine(prompt)) }
      exitCode <- result match {
        case Success(line) => handleInput(line, state, terminal)
        case Failure(err: UserInterruptException) => handleInterrupt(state)
        case Failure(err) => putLogErr(err.getMessage).map(_ => ExitCode.Error)
      }
    } yield exitCode
  }

  def handleInput(
      line: String, 
      state: State,
      terminal: Terminal): IO[ExitCode] = {

    Parser.parseLine(line) match {
      case Left(err)  => putLogErr(err.message).map(_ => ExitCode.Error)
      case Right(cmd) => cmd match {
        case Ls(para) => 
          ls(para, state.project).flatMap(_ => loop(state, terminal))

        case Now(activity) =>
          now(activity, state, terminal)

        case Stop => 
          state.tracked.map { t =>
            stop(state.project, t._1, t._2).flatMap(_ => loop(state, terminal))
          }
          .getOrElse(putStrLn("not tracking")
          .flatMap(_ => loop(state, terminal)))
        
        case Exit =>
          state.tracked
            .map { t =>
              stop(state.project, t._1, t._2)
                .flatMap(_ => loop(state, terminal))
                .map(_ => ExitCode.Success)
            }
            .getOrElse(IO { ExitCode.Success } )

        case Rm(activity)  => 
          store.deleteActivities(activity, state.project).flatMap {
            case Success(_)   => loop(state, terminal)
            case Failure(err) => 
              putLogErr(err.getMessage()).flatMap(_ => loop(state, terminal))
          }

        case _ => 
          putStrLn("unknown command").flatMap(_ => loop(state, terminal))
      }
    }
  }

  def handleInterrupt(state: State) = {
    state match {
      case State(Some((name, time)), project) => 
        stop(project, name, time).flatMap {
          case Success(_)   => IO.pure(ExitCode.Success)
          case Failure(err) => IO.pure(ExitCode.Error)
        }
      case State(None, _) => IO.pure(ExitCode.Success)
    }
  }

  def stop(project: String, activity: String, time: Long): IO[Try[Unit]] = {
    for {
      end <- IO.pure(Instant.now())
      act <- IO.pure(Activity(activity, project, time, end.getEpochSecond))
      written <- store.writeActivity(act)
      result <- written match {
        case Success(_)   => IO.pure(written)
        case Failure(err) => putLogErr(err.getMessage).map(_ => written)
      }
    } yield result
  }

  def now(activity: String, state: State, terminal: Terminal): IO[ExitCode] = {
    for {
      _ <- state match {
        case State(Some((name, time)), project) => stop(project, name, time)
        case _ => IO {}
      }
      _ <- putStrLn(s"tracking ${state.project}/${activity}")
      time <- IO.pure(Instant.now())
      result <- loop(
        state.copy(tracked = Some((activity, time.getEpochSecond()))),
        terminal)
    } yield result
  }

  private def ls(para: Option[TimePara], project: String) = para match {
    case None       => store.readActivitiesFor(project).flatMap(printA)
    case Some(Day)  => lsWith(Activity.onDay _, project)
    case Some(Week) => lsWith(Activity.inWeek _, project)
  }

  private def lsWith(
      fn: (Long, LocalDate, ZoneId) => Boolean, project: String) = {
    
    for {
      date     <- IO { LocalDate.now() }
      zoneId   <- IO { ZoneId.systemDefault() }
      result   <- store.readActivitiesFor(project)
      filtered <- IO { 
        result.map(seq => seq.filter(a => fn(a.from, date, zoneId)))
      }
      effect   <- printA(filtered)
    } yield effect 
  }

  private def printA(result: Try[Seq[Activity]]): IO[Unit] = result match {
    case Success(seq) =>
      IO { println("activities: \n\n" + DisplayText.listSums(seq) + "\n") }
    case Failure(t) =>
      IO { scribe.error(t.getMessage()) }
  }

  private def rm(activity: String, project: String) = {
    store.deleteActivities(activity, project).map {
      case Success(_)   => IO {}
      case Failure(err) => putLogErr(err.getMessage())
    }
  }
}