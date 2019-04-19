package punch.cli

import org.scalatest._

class ParserSpec extends FunSpec with Matchers {
  describe("Parser#parseLine") {
    val check = Parser.parseLine(_)

    it("should parse ls") {
      check("ls") shouldEqual Right(Ls(None))
      check("ls -w") shouldEqual Right(Ls(Some(Week)))
      check("ls -d") shouldEqual Right(Ls(Some(Day)))
    }

    it("should parse now") {
      check("now activity") shouldEqual Right(Now("activity"))
      check("now \"activity\"") shouldEqual Right(Now("activity"))
      check("now 'activity'") shouldEqual Right(Now("activity"))
    }

    it("should parse stop") {
      check("stop") shouldEqual Right(Stop)
    }

    it("should parse exit") {
      check("exit") shouldEqual Right(Exit)
    }

    it("should parse rm") {
      check("rm activity") shouldEqual Right(Rm("activity"))
      check("rm \"activity\"") shouldEqual Right(Rm("activity"))
      check("rm 'activity'") shouldEqual Right(Rm("activity"))
    }

    it("should parse punch") {
      check("punch activity") shouldEqual Right(Punch("activity"))
      check("punch \"activity\"") shouldEqual Right(Punch("activity"))
      check("punch 'activity'") shouldEqual Right(Punch("activity"))
    }

    it("should parse add") {
      val n = None
      val s = (x: Int) => Some(x)

      check("add 10 1-1") shouldEqual Right(Add(10, n, n, 1, n, 1, n))
      check("add 1 10-10") shouldEqual Right(Add(1, n, n, 10, n, 10, n))
      check("add 1 5:01-5:10") shouldEqual Right(Add(1, n, n, 5, s(1), 5, s(10)))
      check("add 1 24:00-5:10") shouldEqual Right(Add(1, n, n, 24, s(0), 5, s(10)))

      check("add 1 0-1").isLeft shouldEqual true
      check("add 1 25-1").isLeft shouldEqual true
      check("add 1 5:1-5:1").isLeft shouldEqual true
      check("add 1 5:1-5:1").isLeft shouldEqual true
      check("add 1 5:60-5:10").isLeft shouldEqual true
      check("add 1 26:10-5:10").isLeft shouldEqual true
      check("add 0 5:10-5:10").isLeft shouldEqual true
      check("add 32 5:10-5:10").isLeft shouldEqual true
      check("add 200 5:10-5:10").isLeft shouldEqual true
    }
  }
}