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
      zoneOffset: java.time.ZoneOffset): Boolean = {
    
    val date = java.time.Instant
      .ofEpochSecond(seconds)
      .atOffset(zoneOffset)
      .toLocalDate()
    
    val dayOfWeek = day.getDayOfWeek.getValue
    val lastDay = day.plusDays(8 - dayOfWeek)
    val firstDay = day.minusDays(dayOfWeek)

    date.isAfter(firstDay) && date.isBefore(lastDay)
  }
}