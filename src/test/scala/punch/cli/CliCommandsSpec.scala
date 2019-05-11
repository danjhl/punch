package punch.cli

import org.scalatest._

class CommandsSpec extends FunSpec with Matchers {
  describe("CliCommands") {
    it("should parse ls") {
      check("ls") shouldEqual Right(LsProjects())
    }

    it("should parse help") {
      check("help") shouldEqual Right(ShowHelp())
      check() shouldEqual Right(ShowHelp())
    }

    it("should parse rm") {
      check("rm", "a") shouldEqual Right(RmProject("a"))
    }

    it("should parse switch") {
      check("project") shouldEqual Right(Switch("project"))
    }

    it("should return unknown command") {
      check("nothing", "really") shouldEqual Left(UnknownCommand())
    }
  }

  def check(args: String*) = CliCommands.interpret(Vector(args : _*))
}