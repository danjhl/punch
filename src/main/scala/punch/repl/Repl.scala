package punch.repl

import punch.io.{Console, ConsoleImpl, Text, RepositoryImpl}
import punch.io.ConsoleImpl.putStrLn
import punch.model.Activity
import punch.cli.Help
import scala.util.{Success, Failure, Try}
import zio.{IO, Task, ZIO}
import org.jline.terminal.{TerminalBuilder, Terminal}
import org.jline.reader.{LineReaderBuilder, LineReader, Candidate}
import org.jline.reader.impl.completer.StringsCompleter
import org.jline.reader.UserInterruptException
import java.time.{LocalDate, Instant, ZoneId, OffsetDateTime}


case class State(tracked: Option[(String, Long)], project: String, exit: Boolean = false)

object Repl {
  private val repo = RepositoryImpl
  private val prompt = s"${Console.BLUE}punch> "

  def start(project: String): Task[Unit] = {
    for {
      terminal <- IO { TerminalBuilder.terminal() }
      reader   <- createReader(terminal)
      _        <- loop(State(None, project), terminal, reader)
    } yield ()
  }

  private def loop(initial: State, terminal: Terminal, reader: LineReader): Task[Unit] = {
    for {
      line  <- IO { reader.readLine(prompt) }.catchSome(onInterrupt(initial))
      state <- onInput(line, initial)
      _     <- if (state.exit) IO { terminal.close() } else loop(state, terminal, reader)
    } yield ()
  }

  private def createReader(terminal: Terminal): Task[LineReader] = {
    for {
      projects   <- repo.readProjects()
      activities <- repo.readActivities()
      reader     <- IO { 
        LineReaderBuilder.builder()
          .completer { (reader, line, candidates) =>
            if (activityCmd(line.line))
              activities.foreach(a => candidates.add(new Candidate(a.name)))
            if (projectCmd(line.line))
              projects.foreach(p => candidates.add(new Candidate(p)))
          }
          .terminal(terminal)
          .build()
      }
    } yield reader
  }

  private def activityCmd(line: String) = line.matches("((now)\\s*?.*)|((rm)\\s*?.*)|((add)\\s*?.*)")
  private def projectCmd(line: String) = line.matches("((punch)\\s*?.*)")

  private def onInput(line: String, state: State): Task[State] = {
    ReplParser.parseLine(line).fold(
      { err => putStrLn(err.message).map(_ => state) },
      {
        case ReplHelp()     => Help.show().map(_ => state)
        case Ls(para)       => ls(para, state).map(_ => state)
        case Now(activity)  => now(activity, state)
        case Stop()         => stopWithMessage(state)
        case Exit()         => stop(state).map(_ => state.copy(exit = true))
        case Sum(param)     => summary(param, state.project).map(_ => state).provide(ConsoleImpl)
        case Agenda(param)  => agenda(param, state.project).map(_ => state).provide(ConsoleImpl)
        case a : Add        => add(a, state)
        case Rm(activity)   => rm(activity, state)
        case Time(activity) => time(activity, state)
        case Punch(project) => stop(state).map(s => s.copy(project = project))
        case _              => putStrLn("unknown command").map(_ => state)
      })
  }

  private def ls(para: Option[LsTimePara], state: State) = para match {
    case None           => repo.readActivitiesFor(state.project).flatMap(printAct)
    case Some(LsDay())  => lsWith(Activity.onDay _, state.project)
    case Some(LsWeek()) => lsWith(Activity.inWeek _, state.project)
  }

  private def lsWith(fn: (Long, LocalDate, ZoneId) => Boolean, project: String) = {
    for {
      date       <- IO { LocalDate.now() }
      zoneId     <- IO { ZoneId.systemDefault() }
      activities <- repo.readActivitiesFor(project)
      seq        <- IO { activities.filter(a => fn(a.from, date, zoneId)) }
      _          <- printAct(seq)
    } yield()
  }

  private def printAct(seq: Seq[Activity]) = {
    putStrLn("\nactivities: \n\n" + Text.listSums(seq) + "\n")
  }

  private def now(activity: String, state: State) = {
    for {
      _    <- stop(state)
      _    <- putStrLn(s"tracking ${state.project}/${activity}")
      time <- IO { Instant.now() }
    } yield state.copy(tracked = Some((activity, time.getEpochSecond())))
  }

  private def stop(state: State): Task[State] = {
    val project = state.project

    state.tracked match {
      case None                => IO.succeed(state)
      case Some((name, start)) => 
        for {
          end <- IO { Instant.now().getEpochSecond }
          _   <- repo.writeActivity(Activity(name, project, start, end))
        } yield state.copy(tracked = None)
    }


  }

  private def stopWithMessage(state: State): Task[State] = state.tracked match {
    case None => stop(state).flatMap(s => putStrLn("not tracking").map(_ => s))
    case _    => stop(state)
  }

  private def rm(activity: String, state: State): Task[State] = {
    repo
      .deleteActivities(activity, state.project)
      .map(_ => state)
  }

  private def time(activity: String, state: State): Task[State] = {
    repo
      .readActivitiesFor(state.project)
      .map(activities => activities.filter(a => a.name == activity))
      .map(activities => activities.map(a => a.seconds))
      .map(activities => activities.sum)
      .map(_ => state)

    for {
      activities <- repo.readActivitiesFor(state.project)
      _          <- {
        val seconds = activities.filter(a => a.name == activity).map(a => a.seconds).sum
        putStrLn(s"tracked ${Text.time(seconds)}")
      }
    } yield state
  }

  private def add(add: Add, state: State): Task[State] = {
    val project = state.project
    val name = add.activityName

    for {
      zoneId <- IO { ZoneId.systemDefault() }
      now    <- IO { Instant.now().atZone(zoneId).toLocalDate() }
      date   <- IO {
        now
          .withYear(add.year.getOrElse(now.getYear()))
          .withMonth(add.month.getOrElse(now.getMonthValue()))
          .withDayOfMonth(add.day.getOrElse(now.getDayOfMonth()))
      }
      fromD  <- IO { date.atTime(add.startHour, add.startMinute.getOrElse(0)) }
      toD    <- IO { date.atTime(add.stopHour, add.stopMinute.getOrElse(0)) }
      offset <- IO { OffsetDateTime.now().getOffset() }
      from   <- IO { fromD.toEpochSecond(offset) }
      to     <- IO { toD.toEpochSecond(offset) }
      _      <- repo.writeActivity(Activity(name, project, from, to))
    } yield state
  }

  private def summary(param: Option[TimePara], project: String): 
      ZIO[Console, Throwable, Unit] = {

    val off = offset(param)
    val printSummary = param match {
      case Some(Week(_)) => SummaryPrinter.printSummaryWeek(_, _, _)
      case _             => SummaryPrinter.printSummary(_, _, _)
    }
    
    for {
      activities <- repo.readActivitiesFor(project)
      zoneId <- IO { ZoneId.systemDefault() }
      date   <- IO { LocalDate.now().minusDays(off) }
      _      <- printSummary(date, zoneId, activities)
    } yield()
  }

  private def offset(param: Option[TimePara]) = param match {
    case None          => 0
    case Some(Day(x))  => x
    case Some(Week(x)) => x * 7
  }

  private def agenda(param: Option[TimePara], project: String): ZIO[Console, Throwable, Unit] = {
    val off = offset(param)
    val printAgenda = param match {
      case Some(Week(_)) => AgendaPrinter.printAgendaWeek(_, _, _)
      case _             => AgendaPrinter.printAgenda(_, _, _)
    }

    for {
      activities <- repo.readActivitiesFor(project)
      zoneId     <- IO { ZoneId.systemDefault() }
      date       <- IO { LocalDate.now().minusDays(off) }
      _          <- printAgenda(date, zoneId, activities)
    } yield()
  }

  private def onInterrupt(state: State): PartialFunction[Throwable, ZIO[Any, Throwable, String]] = { 
    case _ : UserInterruptException => 
      state.tracked match {
        case None => IO.succeed("exit")
        case _    => stop(state).map(_ => "exit")
      }
  }
}