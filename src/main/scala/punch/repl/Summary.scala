package punch.repl

import punch.io.ConsoleImpl.putStrLn
import punch.io.Text
import punch.io.RepositoryImpl
import punch.model.Activity
import scalaz.zio.{IO, Task, ZIO}

import java.time.{LocalDate, Instant, ZoneId, OffsetDateTime}
import java.time.format.DateTimeFormatter
import org.jline.terminal.{TerminalBuilder, Terminal}
import org.jline.reader.{LineReaderBuilder, LineReader}
import org.jline.reader.UserInterruptException

object Summary {
  val repo = RepositoryImpl

  def showSummary(date: LocalDate, zoneId: ZoneId): Task[Unit] = {
    repo
      .readActivities()
      .map { seq =>
        seq.filter(x => Activity.toDate(x.from, zoneId).compareTo(date) == 0)
      }
      .flatMap { seq =>
        if (seq.isEmpty) printSummary(date, "")
        else             printSummary(date, Text.listSums(seq))
      }
  }

  private def printSummary(date: LocalDate, text: String): Task[Unit] = {
    for {
      _ <- putStrLn(s"\n${date.getDayOfWeek.toString} - ${date.toString}")
      _ <- putStrLn("──────────────────────")
      _ <- putStrLn(s"${text}")
    } yield ()
  }

  def showWeekSummary(date: LocalDate): Task[Unit] = {
    val off = date.getDayOfWeek().getValue() - 1
    val start = date.minusDays(off)

    for {
      zoneId <- IO { ZoneId.systemDefault() }
      fn     <- IO { summaryFn(start, zoneId) }
      _      <- (0 to 6).map(fn).reduce((p, n) => p.flatMap(_ => n))
    } yield()
  }

  def summaryFn(date: LocalDate, zoneId: ZoneId): Int => Task[Unit] = { off =>
    val next = date.plusDays(off)
    showSummary(next, zoneId)  
  }
}