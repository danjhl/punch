package punch.repl

import punch.io.Console
import punch.model.Activity
import zio.{IO, Runtime}
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import java.time.{LocalDate, LocalTime, ZoneId, ZoneOffset, Month}

class SummaryPrinterSpec extends FunSpec with Matchers with MockFactory {
  val zoneId = ZoneId.of("UTC")

  describe("SummaryPrinter#printSummaryWeek") {
    it("should print week without activity") {
      val activities = Seq()

      val expected =
        """
        |MONDAY - 2020-03-16
        |──────────────────────
        |
        |
        |TUESDAY - 2020-03-17
        |──────────────────────
        |
        |
        |WEDNESDAY - 2020-03-18
        |──────────────────────
        |
        |
        |THURSDAY - 2020-03-19
        |──────────────────────
        |
        |
        |FRIDAY - 2020-03-20
        |──────────────────────
        |
        |
        |SATURDAY - 2020-03-21
        |──────────────────────
        |
        |
        |SUNDAY - 2020-03-22
        |──────────────────────
        |
        |"""

      checkPrintSummaryWeek(activities, expected)
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
        """
        |MONDAY - 2020-03-16
        |──────────────────────
        |activity · ·  0 h 10 m
        |
        |TUESDAY - 2020-03-17
        |──────────────────────
        |activity1  ·   1 h  0 m
        |activity2  ·   1 h  0 m
        |
        |WEDNESDAY - 2020-03-18
        |──────────────────────
        |activity · ·  0 h  0 m
        |
        |THURSDAY - 2020-03-19
        |──────────────────────
        |activity · ·  0 h  0 m
        |
        |FRIDAY - 2020-03-20
        |──────────────────────
        |activity · ·  6 h  0 m
        |
        |SATURDAY - 2020-03-21
        |──────────────────────
        |
        |
        |SUNDAY - 2020-03-22
        |──────────────────────
        |
        |"""

      checkPrintSummaryWeek(activities, expected)
    }
  }

  describe("SummaryPrinter#printSummary") {
    it("should print summary without activity") {
      val activities = Seq()

      val expected = """
          |FRIDAY - 2020-03-20
          |──────────────────────
          |
          |"""

      checkPrintSummary(activities, expected)
    }

    it("should print summary for single activity") {
      val activities = Seq(
        Activity("activity", "project", date(20, 10, 0), date(20, 10, 20)))

      val expected = """
          |FRIDAY - 2020-03-20
          |──────────────────────
          |activity · ·  0 h 20 m
          |"""

      checkPrintSummary(activities, expected)
    }

    it("should print summary for multiple activities") {
      val activities = Seq(
        Activity("activity1", "project", date(20, 5, 10), date(20, 7, 55)),
        Activity("activity2", "project", date(20, 7, 55), date(20, 8, 15)))

      val expected = """
        |FRIDAY - 2020-03-20
        |──────────────────────
        |activity1  ·   2 h 45 m
        |activity2  ·   0 h 20 m
        |"""

      checkPrintSummary(activities, expected)
    }
  }

  def checkPrintSummaryWeek(activities: Seq[Activity], expected: String) = {
    val console = mock[Console];
    val date = LocalDate.of(2020, Month.MARCH, 20)

    (console.putStr _).expects(expected.stripMargin).returning(IO {})

    val effect = SummaryPrinter.printSummaryWeek(date, zoneId, activities)
      .provide(console)

    Runtime.default.unsafeRun(effect)
  }

  def checkPrintSummary(activities: Seq[Activity], expected: String) = {
    val console = mock[Console];
    val date = LocalDate.of(2020, Month.MARCH, 20)

    (console.putStr _).expects(expected.stripMargin).returning(IO {})

    val effect = SummaryPrinter.printSummary(date, zoneId, activities)
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