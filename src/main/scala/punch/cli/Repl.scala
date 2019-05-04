package punch.cli

import punch.cli.DisplayText._
import scala.util.{Success, Failure, Try}
import scalaz.zio.{IO, Task, ZIO}
import org.jline.terminal.{TerminalBuilder, Terminal}
import org.jline.reader.{LineReaderBuilder, LineReader, Candidate}
import org.jline.reader.impl.completer.StringsCompleter
import org.jline.reader.UserInterruptException
import java.time.{LocalDate, Instant, ZoneId}

case class State(
  tracked: Option[(String, Long)],
  project: String,
  exit: Boolean = false)

object Repl {
  val repo = Repo
  val prompt = s"${Console.BLUE}punch> "

  def start(project: String): Task[Unit] = {
    for {
      terminal <- IO { TerminalBuilder.terminal() }
      _        <- loop(State(None, project), terminal)
    } yield ()
  }

  def loop(initial: State, terminal: Terminal): Task[Unit] = {
    for {
      reader <- createReader(terminal)
      line   <- IO { reader.readLine(prompt) }.catchSome(onInterrupt(initial))
      state  <- onInput(line, initial)
      _      <- if (state.exit) IO.effect() else loop(state, terminal)
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

  def onInput(
      line: String,
      state: State): Task[State] = {

    Parser.parseLine(line).fold(
      { err => putStrLn(err.message).map(_ => state) },
      {
        case Ls(para)      => ls(para, state).map(_ => state)
        case Now(activity) => now(activity, state)
        case Stop          => stopWithMessage(state)
        case Exit          => stop(state).map(_ => state.copy(exit = true))
        case Rm(activity)  => rm(activity, state)
        case _             => putStrLn("unknown command").map(_ => state)
      })
  }

  private def ls(para: Option[TimePara], state: State) = para match {
    case None       => repo.readActivitiesFor(state.project).flatMap(printAct)
    case Some(Day)  => lsWith(Activity.onDay _, state.project)
    case Some(Week) => lsWith(Activity.inWeek _, state.project)
  }

  private def lsWith(
      fn: (Long, LocalDate, ZoneId) => Boolean,
      project: String) = {
    
    for {
      date   <- IO { LocalDate.now() }
      zoneId <- IO { ZoneId.systemDefault() }
      seq    <- repo.readActivitiesFor(project).map {
        _.filter(a => fn(a.from, date, zoneId))
      }
      effect <- printAct(seq)
    } yield effect 
  }

  private def printAct(seq: Seq[Activity]) = {
    putStrLn("activities: \n\n" + DisplayText.listSums(seq) + "\n")
  }

  def now(activity: String, state: State) = {
    for {
      _      <- stop(state)
      _      <- putStrLn(s"tracking ${state.project}/${activity}")
      time   <- IO { Instant.now() }
    } yield state.copy(tracked = Some((activity, time.getEpochSecond())))
  }

  def stop(state: State): Task[State] = {
    state.tracked.map { case (a, time) =>
      for {
        end <- IO { Instant.now() }
        act <- IO { Activity(a, state.project, time, end.getEpochSecond) }
        _   <- repo.writeActivity(act)
      } yield state.copy(tracked = None)
    }
    .getOrElse(IO.succeed(state))
  }

  def stopWithMessage(state: State): Task[State] = state.tracked match {
    case None => stop(state).flatMap(s => putStrLn("not tracking").map(_ => s))
    case _    => stop(state)
  }

  def rm(activity: String, state: State): Task[State] = {
    repo
      .deleteActivities(activity, state.project)
      .map(_ => state)
  }

  def onInterrupt(state: State): 
      PartialFunction[Throwable, ZIO[Any, Throwable, String]] = { 

    case _ : UserInterruptException => 
      state.tracked match {
        case None => IO.succeed("exit")
        case _    => stop(state).map(_ => "exit")
      }
  }
}