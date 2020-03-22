package punch.repl

import punch.io.{Console, Text}
import punch.model.Activity
import zio.{IO, Task, ZIO}
import java.time.{LocalDate, ZoneId}
import java.time.format.DateTimeFormatter

object AgendaPrinter {
  private type ConsoleTask = ZIO[Console, Throwable, Unit]

  def printAgendaWeek(
      date: LocalDate,
      zoneId: ZoneId,
      activities: Seq[Activity]): ConsoleTask = {

    val off = date.getDayOfWeek().getValue() - 1
    val start = date.minusDays(off)
    
    val text = (0 to 6)
      .map(added => agendaForDay(start.plusDays(added), zoneId, activities))
      .mkString
    
    ZIO.accessM[Console](_.putStr(text))
  }

  def printAgenda(
      date: LocalDate,
      zoneId: ZoneId,
      activities: Seq[Activity]): ConsoleTask = {

    val text = agendaForDay(date, zoneId, activities)
    ZIO.accessM[Console](_.putStr(text))
  }

  private def agendaForDay(
      date: LocalDate, zoneId: ZoneId, activities: Seq[Activity]) = {
    
    val formatter = DateTimeFormatter.ofPattern("HH:mm")
    val lines = activities
      .filter(activity => isToday(date, zoneId, activity.from))
      .map(activity => timeFrame(activity, formatter, zoneId))
    
    val text = Text.list(lines)
    
    s"\n${date.getDayOfWeek.toString} - ${date.toString}\n" +
    "──────────────────────\n" +
    s"${text}\n"
  }

  private def isToday(today: LocalDate, zoneId: ZoneId, timestamp: Long) = {
    Activity.toDate(timestamp, zoneId).compareTo(today) == 0
  }

  private def timeFrame(
      activity: Activity, formatter: DateTimeFormatter, zoneId: ZoneId) = {

    val from = Activity.toDateTime(activity.from, zoneId)
    val to = Activity.toDateTime(activity.to, zoneId)
    (activity.name, s"${formatter.format(from)} - ${formatter.format(to)}")
  }
}