package punch.repl

import fastparse._, NoWhitespace._

object ReplParser {
  def parseLine(line: String): Either[ParseError, ReplCommand] = {
    parse(line, Expressions.replCommand(_)) match {
      case Parsed.Success(cmd, _)  => Right(cmd)
      case Parsed.Failure(_, _, _) => Left(ParseError("invalid command"))
    }
  }
}

private object Expressions {
  def replCommand[_ : P]: P[ReplCommand] = P(
    help
    | ls
    | now
    | rm
    | time
    | today
    | add
    | sum
    | agenda
    | punch
    | stop
    | exit
  )

  def help[_ : P]     = P("help").map(_ => ReplHelp())
  def ls[_ : P]       = P("ls" ~/ (ws ~ timeP).?).map(c => Ls(c))
  def stop[_ : P]     = P("stop").!.map(c => Stop())
  def exit[_ : P]     = P("exit").!.map(c => Exit())
  def now[_ : P]      = P("now" ~/ ws ~ (escaped | str)).map(c => Now(c))
  def rm[_ : P]       = P("rm" ~/ ws ~ (escaped | str)).map(c => Rm(c))
  def time[_: P]      = P("time" ~/ ws ~ (escaped | str)).map(c => Time(c))
  def punch[_ : P]    = P("punch" ~/ ws ~ (escaped | str)).map(c => Punch(c))
  def sum[_ : P]      = P("sum" ~/ (ws ~ sumTimeP).? ).map(c => Sum(c))
  def agenda[_ : P]   = P("agenda" ~/ (ws ~ sumTimeP).? ).map(c => Agenda(c))
  def add[_ : P]      = P("add" ~/ ws ~ str ~ ws ~ (date ~ ws).? ~ frame).map(toAdd)
  def today[_ : P]    = P("today" ~/ (ws ~ excludeP).?).map(c => Today(c))

  def timeP[_ : P]    = P(weekP | dayP)
  def weekP[_ : P]    = P("-w").!.map(c => LsWeek())
  def dayP[_ : P]     = P("-d").!.map(c => LsDay())

  def sumTimeP[_ : P] = P(sumWeekP | sumDayP)
  def sumWeekP[_ : P] = P("-w" ~ ("-".? ~ digit ~ digit0.rep).!.?).map(toWeek)
  def sumDayP[_ : P]  = P("-d" ~ ("-".? ~ digit ~ digit0.rep).!.?).map(toDay)

  def excludeP[_ : P] = P("-e" ~ (ws ~ str).rep)

  def str[_ : P]      = P(CharsWhile(_ != ' ')).!
  def ws[_ : P]       = P(CharsWhile(_ == ' '))
  def escaped[_ : P]  = P(escaped1 | escaped2)
  def escaped1[_ : P] = P("\"" ~/ CharsWhile(_ != '"').! ~ "\"")
  def escaped2[_ : P] = P("'" ~/ CharsWhile(_ != '\'').! ~ "'")

  def date[_ : P]     = P(day.! ~ ("." ~/ month.!).? ~ ("." ~/ year.!).?)
  def day[_ : P]      = P(("1" | "2") ~ digit0 | "30" | "31" | digit)
  def month[_ : P]    = P("11" | "12" | digit)
  def year[_ : P]     = P(digit ~ digit0.rep)

  def frame[_ : P]    = P(timeStr ~ "-" ~/ timeStr)
  def timeStr[_ : P]  = P(hour.! ~/ (":" ~/ minutes.!).?)
  def hour[_ : P]     = P("1" ~ digit0 | "2" ~ CharIn("0-4") | digit)
  def minutes[_ : P]  = P(CharIn("1-5") ~ digit0 | "0" ~ digit0)

  def digit[_ : P]    = P(CharIn("1-9"))
  def digit0[_ : P]   = P(digit | "0")


  type os = Option[String]
  type s = String

  val toDay = (c: os) => Day(c.getOrElse("0").toInt)
  val toWeek = (c: os) => Week(c.getOrElse("0").toInt)
  val toAdd = (c: (s, Option[(s, os, os)], (s, os, (s, os)))) => 
    Add(
      c._1,
      c._2.map(d => d._1.toInt),
      c._2.flatMap(m => m._2.map(_.toInt)),
      c._2.flatMap(y => y._3.map(_.toInt)),
      c._3._1.toInt,
      c._3._2.map(_.toInt),
      c._3._3._1.toInt,
      c._3._3._2.map(_.toInt))
}

case class ParseError(message: String)

sealed trait ReplCommand

case class ReplHelp() extends ReplCommand
case class Stop() extends ReplCommand
case class Exit() extends ReplCommand
case class Now(activityName: String) extends ReplCommand
case class Rm(activityName: String) extends ReplCommand
case class Punch(projectName: String) extends ReplCommand
case class Ls(time: Option[LsTimePara]) extends ReplCommand
case class Time(activityName: String) extends ReplCommand
case class Add(
  activityName: String,
  day: Option[Int],
  month: Option[Int],
  year: Option[Int],
  startHour: Int,
  startMinute: Option[Int],
  stopHour: Int,
  stopMinute: Option[Int]) extends ReplCommand
case class Today(excluded: Option[Seq[String]]) extends ReplCommand

sealed trait LsTimePara

case class LsDay() extends LsTimePara
case class LsWeek() extends LsTimePara

case class Sum(time: Option[TimePara]) extends ReplCommand
case class Agenda(time: Option[TimePara]) extends ReplCommand

sealed trait TimePara

case class Day(off: Int) extends TimePara
case class Week(off: Int) extends TimePara