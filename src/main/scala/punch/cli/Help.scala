package punch.cli

import punch.cli.DisplayText._
import scalaz.zio.Task

object Help {
  def show(): Task[Unit] = putStrLn(text)

  private val text =
    """
       |Usage: punch [command]
       |
       |Commands:
       |
       |[project]....................................... start tracking time
       |rm [project].................................... remove project
       |ls.............................................. list projects
       |
       |Project Commands:
       |
       |now [activity].................................. start tracking activity
       |stop............................................ stop tracking activity
       |exit............................................ leave project
       |ls.............................................. list activities
       |rm [activity]................................... remove activity
       |
       |add activity [d[.m][.y]] h[:mm]-h[:mm].......... add activity manually
       |
       |    example: add x 10-11              adds activity today 10:00 to 11:00
       |             add x 10 10:30-11:30     adds activity at 10 of current month
       |             add x 1.10.1999 10-11    adds activity on specified date
       |
       |use <TAB> to autocomplete activity and project names
    """.stripMargin
}