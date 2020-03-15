package punch.io

import punch.model.Activity
import org.scalatest._

class RepositorySpec extends FunSpec with Matchers {
  describe("Repository#parse") {
    it("should parse single line") {
      val str = 
        """{"name": "a", "project": "p", "from": 1, "to": 2, "seconds": 1}"""
      
      RepositoryImpl.parse(str) shouldEqual Seq(Right(Activity("a", "p", 1, 2)))
    }

    it("should parse multiple lines") {
      val str = 
        """{"name": "a", "project": "p", "from": 1, "to": 2, "seconds": 1}
          |{"name": "a2", "project": "p", "from": 1, "to": 2, "seconds": 1}"""
            .stripMargin
      
      RepositoryImpl.parse(str) shouldEqual 
        Seq(Right(Activity("a", "p", 1, 2)), Right(Activity("a2", "p", 1, 2)))
    }

    it("should return single left") {
      val str = """{"name": "a", "project": "p", """
      
      RepositoryImpl.parse(str) shouldEqual Seq(Left("couldn't parse line: 0"))
    }

    it("should return left") {
       val str = 
        """{"name": "a", "project": "p", "from": 1, "to": 2, "seconds": 1}
          |{"name": "a2", "project": "p", "from": 1, "to": 2, seconds": 1}"""
            .stripMargin
      
      RepositoryImpl.parse(str) shouldEqual
        Seq(Right(Activity("a", "p", 1, 2)), Left("couldn't parse line: 1"))
    }
  }

  describe("Repository#convert") {
    it("should convert activity") {
      val a = Activity("name", "project", 1, 2)
      RepositoryImpl.convert(a) shouldEqual 
        """{"name": "name", "project": "project",""" + 
        """ "from": 1, "to": 2, "seconds": 1}"""
    }
  }
}