package punch.repl

import org.scalatest._

class ParserSpec extends FunSpec with Matchers {
  describe("Parser#parseLine") {
    val check = ReplParser.parseLine(_)

    it("should parse help") {
      check("help") shouldEqual Right(ReplHelp())
    }

    it("should parse ls") {
      check("ls") shouldEqual Right(Ls(None))
      check("ls -w") shouldEqual Right(Ls(Some(LsWeek())))
      check("ls -d") shouldEqual Right(Ls(Some(LsDay())))
    }

    it("should parse now") {
      check("now activity") shouldEqual Right(Now("activity"))
      check("now \"activity\"") shouldEqual Right(Now("activity"))
      check("now 'activity'") shouldEqual Right(Now("activity"))
    }

    it("should parse stop") {
      check("stop") shouldEqual Right(Stop())
    }

    it("should parse exit") {
      check("exit") shouldEqual Right(Exit())
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

      check("add a 1-1") shouldEqual 
        Right(Add("a", n, n, n, 1, n, 1, n))

      check("add a 10 1-1") shouldEqual 
        Right(Add("a", s(10), n, n, 1, n, 1, n))

      check("add a 1 10-10") shouldEqual 
        Right(Add("a", s(1), n, n, 10, n, 10, n))

      check("add a 1 5:01-5:10") shouldEqual
        Right(Add("a", s(1), n, n, 5, s(1), 5, s(10)))
        
      check("add a 1 24:00-5:10") shouldEqual
        Right(Add("a", s(1), n, n, 24, s(0), 5, s(10)))

      check("add a 1.1.1999 24:00-5:10") shouldEqual
        Right(Add("a", s(1), s(1), s(1999), 24, s(0), 5, s(10)))

      check("add a 1.1.2019 24:00-5:10") shouldEqual
        Right(Add("a", s(1), s(1), s(2019), 24, s(0), 5, s(10)))

      check("add a 1 0-1").isLeft shouldEqual true
      check("add a 1 25-1").isLeft shouldEqual true
      check("add a 1 5:1-5:1").isLeft shouldEqual true
      check("add a 1 5:1-5:1").isLeft shouldEqual true
      check("add a 1 5:60-5:10").isLeft shouldEqual true
      check("add a 1 26:10-5:10").isLeft shouldEqual true
      check("add a 0 5:10-5:10").isLeft shouldEqual true
      check("add a 32 5:10-5:10").isLeft shouldEqual true
      check("add a 200 5:10-5:10").isLeft shouldEqual true
      check("add a 1.1.0001 5:10-5:10").isLeft shouldEqual true
    }

    it("should parse sum") {
      check("sum") shouldEqual Right(Sum(None))
      check("sum -d") shouldEqual Right(Sum(Some(Day(0))))
      check("sum -d2") shouldEqual Right(Sum(Some(Day(2))))
      check("sum -d-2") shouldEqual Right(Sum(Some(Day(-2))))
      check("sum -d10") shouldEqual Right(Sum(Some(Day(10))))
      check("sum -w") shouldEqual Right(Sum(Some(Week(0))))
      check("sum -w2") shouldEqual Right(Sum(Some(Week(2))))
      check("sum -w-2") shouldEqual Right(Sum(Some(Week(-2))))
      check("sum -w10") shouldEqual Right(Sum(Some(Week(10))))
    }

    it("should parse agenda") {
      check("agenda") shouldEqual Right(Agenda(None))
      check("agenda -d") shouldEqual Right(Agenda(Some(Day(0))))
      check("agenda -d2") shouldEqual Right(Agenda(Some(Day(2))))
      check("agenda -d-2") shouldEqual Right(Agenda(Some(Day(-2))))
      check("agenda -d10") shouldEqual Right(Agenda(Some(Day(10))))
      check("agenda -w") shouldEqual Right(Agenda(Some(Week(0))))
      check("agenda -w2") shouldEqual Right(Agenda(Some(Week(2))))
      check("agenda -w-2") shouldEqual Right(Agenda(Some(Week(-2))))
      check("agenda -w10") shouldEqual Right(Agenda(Some(Week(10))))
    }
  }
}