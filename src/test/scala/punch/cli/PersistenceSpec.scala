package punch.cli

import org.scalatest._

class PersistenceSpec extends FunSpec with Matchers {
  describe("Persistence#parse") {
    it("should parse single line") {
      val str = 
        """{"name": "a", "project": "p", "from": 1, "to": 2, "seconds": 1}"""
      
      Repo.parse(str) shouldEqual Seq(Right(Activity("a", "p", 1, 2)))
    }

    it("should parse multiple lines") {
      val str = 
        """{"name": "a", "project": "p", "from": 1, "to": 2, "seconds": 1}
          |{"name": "a2", "project": "p", "from": 1, "to": 2, "seconds": 1}"""
            .stripMargin
      
      Repo.parse(str) shouldEqual 
        Seq(Right(Activity("a", "p", 1, 2)), Right(Activity("a2", "p", 1, 2)))
    }

    it("should return single left") {
      val str = """{"name": "a", "project": "p", """
      
      Repo.parse(str) shouldEqual Seq(Left("couldn't parse line: 0"))
    }

    it("should return left") {
       val str = 
        """{"name": "a", "project": "p", "from": 1, "to": 2, "seconds": 1}
          |{"name": "a2", "project": "p", "from": 1, "to": 2, seconds": 1}"""
            .stripMargin
      
      Repo.parse(str) shouldEqual
        Seq(Right(Activity("a", "p", 1, 2)), Left("couldn't parse line: 1"))
    }
  }

  describe("Persistence#convert") {
    it("should convert activity") {
      val a = Activity("name", "project", 1, 2)
      Repo.convert(a) shouldEqual 
        """{"name": "name", "project": "project",""" + 
        """ "from": 1, "to": 2, "seconds": 1}"""
    }
  }
}