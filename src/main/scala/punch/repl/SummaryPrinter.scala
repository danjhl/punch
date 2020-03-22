package punch.repl

import punch.io.{Console, Text}
import punch.model.Activity
import zio.ZIO
import java.time.{LocalDate, ZoneId}
import java.time.format.DateTimeFormatter

object SummaryPrinter {
  private type ConsoleTask = ZIO[Console, Throwable, Unit]

  def printSummaryWeek(
      date: LocalDate,
      zoneId: ZoneId,
      activities: Seq[Activity]): ConsoleTask = {

    val off = date.getDayOfWeek().getValue() - 1
    val start = date.minusDays(off)
    
    val text = (0 to 6)
      .map(added => summaryForDay(start.plusDays(added), zoneId, activities))
      .mkString
    
    ZIO.accessM[Console](_.putStr(text))
  }

  def printSummary(
      date: LocalDate,
      zoneId: ZoneId,
      activities: Seq[Activity]): ConsoleTask = {

    val text = summaryForDay(date, zoneId, activities)
    ZIO.accessM[Console](_.putStr(text))
  }

  private def summaryForDay(
      date: LocalDate, zoneId: ZoneId, activities: Seq[Activity]) = {

    val today = activities
      .filter(activity => isToday(date, zoneId, activity.from))
    
    val text = Text.listSums(today)

    s"\n${date.getDayOfWeek.toString} - ${date.toString}\n" +
    "──────────────────────\n" +
    s"${text}\n"
  }

  private def isToday(today: LocalDate, zoneId: ZoneId, timestamp: Long) = {
    Activity.toDate(timestamp, zoneId).compareTo(today) == 0
  }
}