package punch.cli

import fastparse._, NoWhitespace._

object Parser {
  def parseLine(line: String): Either[ParseError, ReplCommand] = {
    parse(line, Expressions.replCommand(_)) match {
      case Parsed.Success(cmd, _)  => Right(cmd)
      case Parsed.Failure(_, _, _) => Left(ParseError("fail"))
    }
  }
}

private object Expressions {
  def replCommand[_ : P]: P[ReplCommand] = P(
    ls
    | now
    | rm
    | add
    | punch
    | stop
    | exit
  )

  def ls[_ : P]       = P("ls" ~ (" " ~ timeP).?).map(c => Ls(c))
  def stop[_ : P]     = P("stop").!.map(c => Stop)
  def exit[_ : P]     = P("exit").!.map(c => Exit)
  def now[_ : P]      = P("now" ~/ " " ~ (escaped | str)).map(c => Now(c))
  def rm[_ : P]       = P("rm" ~/ " " ~ (escaped | str)).map(c => Rm(c))
  def punch[_ : P]    = P("punch" ~/ " " ~ (escaped | str)).map(c => Punch(c))
  def add[_ : P]      = P("add" ~/ " " ~ date ~/ " " ~ frame).map(toAdd)

  def timeP[_ : P]    = P(weekP | dayP)
  def weekP[_ : P]    = P("-w").!.map(c => Week)
  def dayP[_ : P]     = P("-d").!.map(c => Day)

  def str[_ : P]      = P(CharsWhile(_ != ' ')).!
  def escaped[_ : P]  = P(escaped1 | escaped2)
  def escaped1[_ : P] = P("\"" ~/ CharsWhile(_ != '"').! ~ "\"")
  def escaped2[_ : P] = P("'" ~/ CharsWhile(_ != '\'').! ~ "'")

  def date[_ : P]     = P(day.! ~ ("." ~/ month.!).? ~ ("." ~/ year.!).?)
  def day[_ : P]      = P(("1" | "2") ~ digit0 | "30" | "31" | digit)
  def month[_ : P]    = P("11" | "12" | digit)
  def year[_ : P]     = P(digit.rep)

  def frame[_ : P]    = P(time ~ "-" ~/ time)
  def time[_ : P]     = P(hour.! ~/ (":" ~/ minutes.!).?)
  def hour[_ : P]     = P("1" ~ digit0 | "2" ~ CharIn("0-4") | digit)
  def minutes[_ : P]  = P(CharIn("1-5") ~ digit0 | "0" ~ digit0)

  def digit[_ : P]    = P(CharIn("1-9"))
  def digit0[_ : P]   = P(digit | "0")


  type os = Option[String]
  type s = String

  val toAdd = (c: (s, os, os, (s, os, (s, os)))) => 
    Add(c._1.toInt,
        c._2.map(_.toInt),
        c._3.map(_.toInt),
        c._4._1.toInt,
        c._4._2.map(_.toInt),
        c._4._3._1.toInt,
        c._4._3._2.map(_.toInt))
}

case class ParseError(message: String)

sealed trait ReplCommand
case object Stop extends ReplCommand
case object Exit extends ReplCommand
case class Now(activityName: String) extends ReplCommand
case class Rm(activityName: String) extends ReplCommand
case class Punch(projectName: String) extends ReplCommand
case class Add(day: Int,
               month: Option[Int],
               year: Option[Int],
               startHour: Int,
               startMinute: Option[Int],
               stopHour: Int,
               stopMinute: Option[Int]) extends ReplCommand

case class Ls(time: Option[TimePara]) extends ReplCommand

sealed trait TimePara
case object Day extends TimePara
case object Week extends TimePara
