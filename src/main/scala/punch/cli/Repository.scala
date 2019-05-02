package punch.cli

import scalaz.zio.{IO, Task}
import scala.util.{Try, Success, Failure}
import java.nio.file.{Files, Paths, StandardOpenOption}

trait Repository {
  def writeActivity(activity: Activity): Task[Unit]
  def readActivities(): Task[Seq[Activity]]
  def readActivitiesFor(project: String): Task[Seq[Activity]]
  def deleteActivities(name: String, project: String): Task[Unit]
  def deleteProject(name: String): Task[Unit]
}

object Repo extends Repository {
  private val file = "act.db"
  private val activityPattern = 
    ("""\{\s*"name"\s*:\s*"(.*?)"\s*,\s*""" +
    """"project"\s*:\s*"(.*?)"\s*,\s*""" +
    """"from"\s*:\s*(\d*)\s*,\s*""" +
    """"to"\s*:\s*(\d*)\s*,\s*""" +
    """"seconds"\s*:\s*(\d*)\}""").r

  def writeActivity(activity: Activity): Task[Unit] = {
    val bytes = ("\n" + convert(activity)).getBytes()
    val option = StandardOpenOption.APPEND

    IO.effect(Paths.get(file))
      .flatMap(path => IO.effect(Files.write(path, bytes, option)))
  }

  def convert(activity: Activity): String = {
    val a = activity
    s"""{"name": "${a.name}", "project": "${a.project}", """ +
      s""""from": ${a.from}, "to": ${a.to}, "seconds": ${a.seconds}}"""
  }

  def readActivities(): Task[Seq[Activity]] = {
    for {
      path   <- IO { Paths.get(file) }
      bytes  <- IO { Files.readAllBytes(path) }
      result <- {
        val seq = parse(new String(bytes))
        val lefts = seq.filter(_.isLeft)

        if (lefts.isEmpty) IO.succeed(seq.map(_.right.get))
        else               IO.fail(new Exception(lefts.mkString("\n")))
      }
    } yield result
  }

  def readActivitiesFor(project: String): Task[Seq[Activity]] = {
    readActivities().map { seq =>
      seq.filter(_.project == project)
    }
  }

  def parse(str: String): Seq[Either[String, Activity]] = {
    if (str.isEmpty) {
      Nil
    }
    else {
      str.split("\n").zipWithIndex.map {
        case (activityPattern(name, project, from, to, _), _) =>
          Right(Activity(name, project, from.toLong, to.toLong))
        case (_, index) =>
          Left(s"couldn't parse line: $index")
      }
    }
  }

  def deleteActivities(activity: String, project: String): Task[Unit] = {
    delete(a => a.name == activity && a.project == project)
  }

  def deleteProject(project: String): Task[Unit] = {
    delete(a => a.project == project)
  }

  private def delete(filter: Activity => Boolean): Task[Unit] = {
    readActivities().map { seq =>
      seq
        .filter(a => !filter(a))
        .map(convert)
        .mkString("\n")
    }
    .flatMap { str =>
      IO.effect(Paths.get(file))
        .flatMap(path => IO.effect(Files.write(path, str.getBytes())))
    }
  }
}