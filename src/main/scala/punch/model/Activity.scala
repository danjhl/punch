package punch.model

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneId

case class Activity(
    name: String,
    project: String,
    from: Long,
    to: Long) {
  
  val seconds: Long = to - from
}

object Activity {
  def inWeek(seconds: Long, day: LocalDate, zoneId: ZoneId): Boolean = {
    val date = toDate(seconds, zoneId)
    val dayOfWeek = day.getDayOfWeek.getValue
    val lastDay = day.plusDays(8 - dayOfWeek)
    val firstDay = day.minusDays(dayOfWeek)

    date.isAfter(firstDay) && date.isBefore(lastDay)
  }

  def onDay(seconds: Long, day: LocalDate, zoneId: ZoneId): Boolean = {
    val date = toDate(seconds, zoneId)
    date.compareTo(day) == 0
  }

  def toDate(seconds: Long, zoneId: ZoneId): LocalDate = {
    java.time.Instant
      .ofEpochSecond(seconds)
      .atZone(zoneId)
      .toLocalDate()
  }

  def toDateTime(seconds: Long, zoneId: ZoneId): LocalDateTime = {
    java.time.Instant
      .ofEpochSecond(seconds)
      .atZone(zoneId)
      .toLocalDateTime()
  }
}