package punch.repl

import punch.io.Console
import punch.model.Activity
import zio.{IO, Runtime}
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import java.time.{LocalDate, LocalTime, ZoneId, ZoneOffset, Month}

class AgendaPrinterSpec extends FunSpec with Matchers with MockFactory {
  val zoneId = ZoneId.of("UTC")

  describe("AgendaPrinter#printAgendaWeek") {
    it("should print week without activity") {
      val activities = Seq()

      val expected =
        "\n" +
        "MONDAY - 2020-03-16\n" +
        "──────────────────────\n\n\n" +
        "TUESDAY - 2020-03-17\n" +
        "──────────────────────\n\n\n" +
        "WEDNESDAY - 2020-03-18\n" +
        "──────────────────────\n\n\n" +
        "THURSDAY - 2020-03-19\n" +
        "──────────────────────\n\n\n" +
        "FRIDAY - 2020-03-20\n" +
        "──────────────────────\n\n\n" +
        "SATURDAY - 2020-03-21\n" +
        "──────────────────────\n\n\n" +
        "SUNDAY - 2020-03-22\n" +
        "──────────────────────\n\n"

      checkPrintAgendaWeek(activities, expected)
    }

    it("should print week with activities") {
      val activities = Seq(
        Activity("activity", "project", date(16, 12, 20), date(16, 12, 30)),
        Activity("activity1", "project", date(17, 13, 20), date(17, 14, 20)),
        Activity("activity2", "project", date(17, 14, 20), date(17, 15, 20)),
        Activity("activity", "project", date(18, 15, 20), date(18, 15, 20)),
        Activity("activity", "project", date(19, 15, 20), date(19, 15, 20)),
        Activity("activity", "project", date(20, 22, 0), date(21, 4, 0)))

      val expected =
        "\n" +
        "MONDAY - 2020-03-16\n" +
        "──────────────────────\n" +
        "activity · · 12:20 - 12:30\n" +
        "\n" +
        "TUESDAY - 2020-03-17\n" +
        "──────────────────────\n" +
        "activity1  ·  13:20 - 14:20\n" +
        "activity2  ·  14:20 - 15:20\n" +
        "\n" +
        "WEDNESDAY - 2020-03-18\n" +
        "──────────────────────\n" +
        "activity · · 15:20 - 15:20\n" +
        "\n" +
        "THURSDAY - 2020-03-19\n" +
        "──────────────────────\n" +
        "activity · · 15:20 - 15:20\n" +
        "\n" +
        "FRIDAY - 2020-03-20\n" +
        "──────────────────────\n" +
        "activity · · 22:00 - 04:00\n" +
        "\n" +
        "SATURDAY - 2020-03-21\n" +
        "──────────────────────\n" +
        "\n" +
        "\n" +
        "SUNDAY - 2020-03-22\n" +
        "──────────────────────\n\n"

      checkPrintAgendaWeek(activities, expected)
    }
  }

  describe("AgendaPrinter#printAgenda") {
    it("should print agenda for no activities") {
      val activities = Seq()

      val expected =
        "\n" +
        "FRIDAY - 2020-03-20\n" +
        "──────────────────────\n\n"

      checkPrintAgenda(activities, expected)
    }

    it("should print agenda for single activity") {
      val activities = Seq(
        Activity("activity", "project", date(20, 22, 0), date(20, 23, 0)))

      val expected =
        "\n" +
        "FRIDAY - 2020-03-20\n" +
        "──────────────────────\n" +
        "activity · · 22:00 - 23:00\n"

      checkPrintAgenda(activities, expected)
    }

    it("should print agenda for multiple activities") {
      val activities = Seq(
        Activity("activity1", "project", date(20, 4, 25), date(20, 7, 10)),
        Activity("activity2", "project", date(20, 22, 0), date(20, 23, 0)))

      val expected =
        "\n" +
        "FRIDAY - 2020-03-20\n" +
        "──────────────────────\n" +
        "activity1  ·  04:25 - 07:10\n" +
        "activity2  ·  22:00 - 23:00\n"

      checkPrintAgenda(activities, expected)
    }
  }

  def checkPrintAgendaWeek(activities: Seq[Activity], expected: String) = {
    val console = mock[Console];

    (console.putStr _).expects(expected.stripMargin).returning(IO {})

    val date = LocalDate.of(2020, Month.MARCH, 20)
    val effect = AgendaPrinter.printAgendaWeek(date, zoneId, activities)
      .provide(console)

    Runtime.default.unsafeRun(effect)
  }

  def checkPrintAgenda(activities: Seq[Activity], expected: String) = {
    val console = mock[Console];

    (console.putStr _).expects(expected.stripMargin).returning(IO {})

    val date = LocalDate.of(2020, Month.MARCH, 20)
    val effect = AgendaPrinter.printAgenda(date, zoneId, activities)
      .provide(console)

    Runtime.default.unsafeRun(effect)
  }

  def date(day: Int, hour: Int, minute: Int) = {
    LocalDate
      .of(2020, Month.MARCH, day)
      .atTime(hour, minute)
      .toEpochSecond(ZoneOffset.UTC)
  }
}