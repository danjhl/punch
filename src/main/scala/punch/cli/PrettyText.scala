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
    val rightWidth = activities.map(_.timeInSeconds.toString.length).max

    activities.map(single(_, leftWidth, rightWidth)).mkString("\n")
  }

  private def single(a: Activity, leftWidth: Int, rightWidth: Int) = {
    val padding1 = paddingDotted(leftWidth, a.name.length)
    val padding2 = padding(rightWidth, a.timeInSeconds.toString.length)
    s"${a.name}${padding1}${padding2}${a.timeInSeconds}"
  }

  private def paddingDotted(width: Int, strLen: Int) = {
    val uneven = (_: Int) % 2 == 1
    val begin = (_: Int) == strLen
    val end = (_: Int) == width
    val dot = (n: Int) => if (uneven(n) || begin(n) || end(n)) " " else "·"

    (strLen to width).map(dot).mkString
  }

  private def padding(width: Int, strLen: Int) = {
    (strLen until width).map(_ => " ").mkString
  }
}