package punch.cli

import fastparse._, NoWhitespace._


object Parser {
  def parseLine(line: String): Either[ParseError, ReplCommand] = {
    parse(line, Expressions.command(_)) match {
      case Parsed.Success(cmd, _)  => Right(cmd)
      case Parsed.Failure(_, _, _) => Left(ParseError("fail"))
    }
  }
}

private object Expressions {
  def command[_ : P]: P[ReplCommand] = P(
    ls
    | now
    | rm
    | add
    | punch
    | stop
    | exit
  )

  def ls[_ : P]       = P("ls").!.map(c => Ls)
  def stop[_ : P]     = P("stop").!.map(c => Stop)
  def exit[_ : P]     = P("exit").!.map(c => Exit)
  def now[_ : P]      = P("now" ~/ " " ~ (escaped | str)).map(c => Now(c))
  def rm[_ : P]       = P("rm" ~/ " " ~ (escaped | str)).map(c => Rm(c))
  def punch[_ : P]    = P("punch" ~/ " " ~ (escaped | str)).map(c => Punch(c))
  def add[_ : P]      = P("add" ~/ " " ~ date ~/ " " ~ frame).map(toAdd)

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
final case object Ls extends ReplCommand
final case object Stop extends ReplCommand
final case object Exit extends ReplCommand
final case class Now(activityName: String) extends ReplCommand
final case class Rm(activityName: String) extends ReplCommand
final case class Punch(projectName: String) extends ReplCommand
final case class Add(day: Int,
                     month: Option[Int],
                     year: Option[Int],
                     startHour: Int,
                     startMinute: Option[Int],
                     stopHour: Int,
                     stopMinute: Option[Int]) extends ReplCommand