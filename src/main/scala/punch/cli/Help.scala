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
       |[project]................... start tracking time
       |card........................ show callendar
       |rm [project]................ remove project
       |ls.......................... list projects
       |
       |Project Commands:
       |
       |now [activity].............. start tracking activity
       |stop........................ stop tracking activity
       |exit........................ leave project
       |ls.......................... list activities
       |rm [activity]............... remove activity
       |rm [activityA activityB...]. remove activities
       |
       |use <TAB> to autocomplete activity and project names
    """.stripMargin
}