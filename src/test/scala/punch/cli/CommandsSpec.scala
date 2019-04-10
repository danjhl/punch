package punch.cli

import org.scalatest._

class CommandsSpec extends FunSpec with Matchers {
  describe("Commands") {
    it("should parse help") {
      check("help") shouldEqual Right(HelpCmd)
    }

    it("should parse switch") {
      check("projectName") shouldEqual Right(SwitchCmd("projectName"))
    }

    it("should return unknown command") {
      check("nothing", "really") shouldEqual Left(UnknownCommand)
    }
  }

  def check(args: String*) = Commands.parse(Vector(args : _*))
}