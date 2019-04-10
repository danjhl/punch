package punch.cli

import cats.effect.IO
import scala.util.{Try, Success}

object Persistence {
  def readActivities(): IO[Try[Seq[Activity]]] = {
    IO { Success(Vector(
      Activity("actA", "projectA", 1000, 1100),
      Activity("actA", "projectA", 1000, 11000),
      Activity("actA", "projectA", 1000, 11000),
      Activity("actAWithALongName", "projectA", 1000, 1100000),
    )) }
  }
}