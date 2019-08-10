package klk

import cats.effect.IO

object RunTest
extends SimpleTest
{
  test("something") {
    IO.pure(assert("value is true")(true))
  }
}
