package punch.cli

import org.scalatest._

class ActivitySpec extends FunSpec with Matchers {
  describe("Activity") {
    it("should return the spend time") {
      val a = Activity("activity", "project", 100L, 110L)
      a.timeInSeconds shouldEqual 10L
    }
  }
}