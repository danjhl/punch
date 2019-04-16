package punch.cli

object DisplayText {
  def listActivities(activities: Seq[Activity]): String = {
    list(activities.map(a => (a.name, a.seconds.toString)))
  }

  def list(seq: Seq[(String, String)]): String = {
    if (seq.isEmpty)
      ""
    else
      listNonEmpty(seq)
  }

  def listNonEmpty(seq: Seq[(String, String)]) = {
    val leftWidth = seq.map(_._1.length).max
    val rightWidth = seq.map(_._2.length).max
    seq.map(row(_, leftWidth, rightWidth)).mkString("\n")
  }

  def row(r: (String, String), leftWidth: Int, rightWidth: Int) = {
    val (left, right) = r
    val end = leftWidth + rightWidth - right.length
    val padding = paddingDotted(left.length, end)
    s"${left}${padding}${right}"
  }

  private def paddingDotted(start: Int, end: Int) = {
    val uneven = (_: Int) % 2 == 0
    val first = (_: Int) == start
    val last = (_: Int) == end
    val dot = (n: Int) => if (uneven(n) || first(n) || last(n)) " " else "·"

    (start to end).map(dot).mkString
  }
}