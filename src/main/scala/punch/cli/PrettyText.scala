package punch.cli

object PrettyText {
  def pretty(activities: Seq[Activity]): String = {
    if (activities.isEmpty) 
      ""
    else 
      prettyNonEmpty(activities)
  }

  private def prettyNonEmpty(activities: Seq[Activity]): String = {
    val leftWidth = activities.map(_.name.length).max
    val rightWidth = activities.map(_.seconds.toString.length).max

    activities.map(single(_, leftWidth, rightWidth)).mkString("\n")
  }

  private def single(a: Activity, leftWidth: Int, rightWidth: Int) = {
    val end = leftWidth + rightWidth - a.seconds.toString.length
    val padding = paddingDotted(a.name.length, end)
    s"${a.name}${padding}${a.seconds}"
  }

  private def paddingDotted(start: Int, end: Int) = {
    val uneven = (_: Int) % 2 == 0
    val first = (_: Int) == start
    val last = (_: Int) == end
    val dot = (n: Int) => if (uneven(n) || first(n) || last(n)) " " else "·"

    (start to end).map(dot).mkString
  }
}