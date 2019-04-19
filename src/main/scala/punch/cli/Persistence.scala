package punch.cli

import cats.effect.IO
import scala.util.{Try, Success, Failure}
import java.nio.file.{Files, Paths, StandardOpenOption}

trait Store {
  def writeActivity(activity: Activity): IO[Try[Unit]]
  def readActivities(): IO[Try[Seq[Activity]]]
  def deleteActivities(name: String): IO[Try[Unit]]
  // deleteProject(name: String): IO[Try[Unit]]
}

object Persistence extends Store {
  private val file = "act.db"
  private val activityPattern = 
    ("""\{\s*"name"\s*:\s*"(.*?)"\s*,\s*""" +
    """"project"\s*:\s*"(.*?)"\s*,\s*""" +
    """"from"\s*:\s*(\d*)\s*,\s*""" +
    """"to"\s*:\s*(\d*)\s*,\s*""" +
    """"seconds"\s*:\s*(\d*)\}""").r

  def writeActivity(activity: Activity): IO[Try[Unit]] = {
    IO {
      Try(Files.write(Paths.get(file),
        ("\n" + convert(activity)).getBytes(), StandardOpenOption.APPEND))
    }
  }

  def convert(activity: Activity): String = {
    val a = activity
    s"""{"name": "${a.name}", "project": "${a.project}", """ +
      s""""from": ${a.from}, "to": ${a.to}, "seconds": ${a.seconds}}"""
  }

  def readActivities(): IO[Try[Seq[Activity]]] = {
    IO {
      for {
        json       <- Try(new String(Files.readAllBytes(Paths.get(file))))
        result     <- Try(parse(json))
        _          <- Try(result.collect { case Left(err) => scribe.error(err) })
        activities <- Try(result.collect { case Right(a) => a })
      } yield activities
    }
  }

  def parseLine(line: String, index: Int): Either[String, Activity] = {
    line match {
      case activityPattern(name, project, from, to, _) =>
        Right(Activity(name, project, from.toLong, to.toLong))
      case _ =>
        Left(s"couldn't parse line: $index")
    }
  }

  def parse(str: String): Seq[Either[String, Activity]] = {
    if (str.isEmpty)
      Nil
    else
      str.split("\n").zipWithIndex.map(e => parseLine(e._1, e._2))
  }

  def deleteActivities(activity: String): IO[Try[Unit]] = {
    for {
      result     <- readActivities()
      activities <- result match {
                      case Success(activities) => IO { activities }
                      case Failure(err) => IO { scribe.error(err.getMessage); Nil }
                    }
      filtered   <- IO { activities.filter(_.name != activity).map(convert).mkString("\n") }
      res          <- IO {
                      Try(Files.write(Paths.get(file), filtered.getBytes)).map(_ => {})
                    }
    } yield res
  }
}