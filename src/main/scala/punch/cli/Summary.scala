package punch.cli

import DisplayText.putStrLn
import scalaz.zio.{IO, Task, ZIO}

import java.time.{LocalDate, Instant, ZoneId, OffsetDateTime}
import org.jline.terminal.{TerminalBuilder, Terminal}
import org.jline.reader.{LineReaderBuilder, LineReader}
import org.jline.reader.UserInterruptException

object Summary {
  val repo = Repo

  def showSummary(date: LocalDate, zoneId: ZoneId): Task[Unit] = {
    repo
      .readActivities()
      .map { seq => 
        seq.filter(x => Activity.toDate(x.from, zoneId).compareTo(date) == 0)
      }
      .map(DisplayText.listSums)
      .flatMap(text => printSummary(date, text))
  }

  private def printSummary(date: LocalDate, text: String): Task[Unit] = {
    for {
      _ <- putStrLn(s"\n${date.getDayOfWeek.toString} - ${date.toString}\n")
      _ <- putStrLn("──────────────────────\n")
      _ <- putStrLn(s"${text}\n")
    } yield ()
  }

  def showWeekSummary(date: LocalDate): Task[Unit] = {
    val off = date.getDayOfWeek().getValue() - 1
    val start = date.minusDays(off)

    for {
      zoneId <- IO { ZoneId.systemDefault() }
      fn     <- IO { summaryFn(start, zoneId) }
      _      <- (0 to 5).map(fn).reduce((p, n) => p.flatMap(_ => n))
    } yield()
  }

  def summaryFn(date: LocalDate, zoneId: ZoneId): Int => Task[Unit] = { off =>
    val next = date.plusDays(off)
    showSummary(next, zoneId)  
  }
}