package punch.cli

import org.scalatest._
import java.time.{LocalDate, LocalTime, ZoneOffset, Month}

class ActivitySpec extends FunSpec with Matchers {
  describe("Activity") {
    it("should return the spend time") {
      val a = Activity("activity", "project", 100L, 110L)
      a.seconds shouldEqual 10L
    }
  }

  describe("Activity#inWeek") {
    val check = (day: Int, weekDay: Int) => {
      val weekDayDate = LocalDate.of(2019, Month.APRIL, weekDay);
      val dayDate = LocalDate.of(2019, Month.APRIL, day)
        .toEpochSecond(LocalTime.MIN, ZoneOffset.MIN)

      Activity.inWeek(dayDate, weekDayDate, ZoneOffset.MIN)
    }

    it("should return true for day in week") {
      check(11, 10) shouldEqual true
      check(11, 11) shouldEqual true
      check(14, 11) shouldEqual true
      check(8, 11) shouldEqual true
      check(11, 14) shouldEqual true
      check(11, 8) shouldEqual true
    }

    it("should return false for day after week") {
      check(15, 11) shouldEqual false
    }

    it("should return false for day before week") {
      check(7, 11) shouldEqual false
    }
  }
}