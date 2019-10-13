package klk

import cats.effect.IO

class BasicTest
extends IOTest
{
  test("basic sbt test")(IO.pure(1 == 1))
}
