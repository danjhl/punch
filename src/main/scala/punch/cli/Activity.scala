package punch.cli

case class Activity(
    name: String,
    project: String,
    from: Long,
    to: Long) {
  
  val seconds: Long = to - from
}

object Activity {
  def inWeek(
      seconds: Long, 
      day: java.time.LocalDate, 
      zoneId: java.time.ZoneId): Boolean = {
    
    val date = java.time.Instant
      .ofEpochSecond(seconds)
      .atZone(zoneId)
      .toLocalDate()
    
    val dayOfWeek = day.getDayOfWeek.getValue
    val lastDay = day.plusDays(8 - dayOfWeek)
    val firstDay = day.minusDays(dayOfWeek)

    date.isAfter(firstDay) && date.isBefore(lastDay)
  }

  def onDay(
      seconds: Long, 
      day: java.time.LocalDate, 
      zoneId: java.time.ZoneId): Boolean = {
    
    val date = java.time.Instant
      .ofEpochSecond(seconds)
      .atZone(zoneId)
      .toLocalDate()

    date.compareTo(day) == 0
  }
}