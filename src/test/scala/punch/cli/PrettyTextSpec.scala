package punch.cli

import org.scalatest._

class PrettyTextSpec extends FunSpec with Matchers {
  describe("PrettyText") {
    val check = PrettyText.pretty(_)

    it("should prettyfy activity") {
      check(Vector(Activity("a", "p", 100, 200))) shouldEqual "a 100"
    }

    it("should prettyfy multiple activities") {
      val v = Vector(Activity("alongname", "p", 10, 100010),
                     Activity("a", "p", 100, 300),
                     Activity("ashort", "p", 10, 1010),
                     Activity("ashort", "p", 10, 100010))

      check(v) shouldEqual """|alongname 100000
                              |a  · · · · · 200
                              |ashort · ·  1000
                              |ashort ·  100000""".stripMargin
    }

    it("should prettyfy empty seq") {
      check(Vector()) shouldEqual ""
    }
  }
}