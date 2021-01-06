package punch.io

import punch.model.Activity
import org.scalatest._

class TextSpec extends FunSpec with Matchers {
  describe("Text#listActivities") {
    val check = Text.listActivities(_)

    it("should prettyfy activity") {
      check(Vector(Activity("a", "p", 100, 200))) shouldEqual "a  ·  100"
    }

    it("should prettyfy multiple activities") {
      val v = Vector(
        Activity("alongname", "p", 10, 100010),
        Activity("a", "p", 100, 300),
        Activity("ashort", "p", 10, 1010),
        Activity("other", "p", 10, 100010))

      check(v) shouldEqual 
        "alongname  ·  100000\n" +
        "a  · · · · · · · 200\n" +
        "ashort · · · ·  1000\n" +
        "other  · · ·  100000"
    }

    it("should prettyfy empty seq") {
      check(Vector()) shouldEqual ""
    }
  }

  describe("Text#list") {
    val check = Text.list(_)

    it("should create list of empty seq") {
      check(Seq()) shouldEqual ""
    }

    it("should create list of seq") {
      val s = Seq(
        ("alongname", "100000"),
        ("a", "200"),
        ("ashort", "1000"),
        ("ashort", "100000"))

      check(s) shouldEqual 
        "alongname  ·  100000\n" +
        "a  · · · · · · · 200\n" + 
        "ashort · · · ·  1000\n" +
        "ashort · · ·  100000" 
    }
  }

  describe("Text#listSum") {
    val check = Text.listSums(_)

    it("should calculate and create a list of sums") {
      val v = Vector(
        Activity("onelongname", "p", 10, 100010),
        Activity("onelongname", "p", 100, 300),
        Activity("two", "p", 10, 20),
        Activity("two", "p", 10, 20))

      check(v) shouldEqual
        "onelongname  ·  27 h 50 m\n" +
        "two  · · · · ·   0 h  0 m"
    }
  }
}