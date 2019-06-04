package punch.cli

import scalaz.zio.{IO, Task}
import scala.util.{Try, Success, Failure}
import java.nio.file.{Files, Paths, StandardOpenOption, LinkOption, Path}

trait Repository {
  def writeActivity(activity: Activity): Task[Unit]
  def readActivities(): Task[Seq[Activity]]
  def readActivitiesFor(project: String): Task[Seq[Activity]]
  def deleteActivities(name: String, project: String): Task[Unit]
  def readProjects(): Task[Seq[String]]
  def deleteProject(name: String): Task[Unit]
}

object Repo extends Repository {

  private val dir = ".punch"
  private val file = "punch.store"

  private val activityPattern = 
    ("""\{\s*"name"\s*:\s*"(.*?)"\s*,\s*""" +
    """"project"\s*:\s*"(.*?)"\s*,\s*""" +
    """"from"\s*:\s*(\d*)\s*,\s*""" +
    """"to"\s*:\s*(\d*)\s*,\s*""" +
    """"seconds"\s*:\s*(\d*)\}""").r

  def writeActivity(activity: Activity): Task[Unit] = {
    val bytes = (convert(activity) + "\n").getBytes()
    val option = StandardOpenOption.APPEND

    for {
      path <- getPath()
      _    <- createFileIfNotExists(path)
      _    <- IO { Files.write(path, bytes, option) }
    } yield ()
  }

  def convert(activity: Activity): String = {
    val a = activity
    s"""{"name": "${a.name}", "project": "${a.project}", """ +
      s""""from": ${a.from}, "to": ${a.to}, "seconds": ${a.seconds}}"""
  }

  def readActivities(): Task[Seq[Activity]] = {
    for {
      path   <- getPath()
      _      <- createFileIfNotExists(path)
      bytes  <- IO { Files.readAllBytes(path) }
      result <- {
        val seq = parse(new String(bytes))
        val lefts = seq.filter(_.isLeft)

        if (lefts.isEmpty) IO.succeed(seq.map(_.right.get))
        else               IO.fail(new Exception(lefts.mkString("\n")))
      }
    } yield result
  }

  def getPath(): Task[Path] = {
    for {
      home <- IO { System.getProperty("user.home") }
      path <- IO { Paths.get(home, dir, file) }
    } yield path
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
      for {
        path <- getPath()
        _    <- createFileIfNotExists(path)
        _    <- IO { Files.write(path, str.getBytes()) }
      } yield ()
    }
  }

  def readProjects(): Task[Seq[String]] = {
    readActivities().map(seq => seq.map(_.project).toSet.toSeq)
  }

  private def createFileIfNotExists(path: Path): Task[Unit] = {
    for {
      exists <- IO { Files.exists(path, LinkOption.NOFOLLOW_LINKS) }
      _      <- if (!exists) createFileAndDirectories(path) else IO.succeed()
    } yield ()
  }

  private def createFileAndDirectories(path: Path): Task[Unit] = {
    IO.effect(Files.createDirectories(path.getParent()))
      .flatMap(_ => IO.effect(Files.createFile(path)))
  }
}