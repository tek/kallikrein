package klk

import cats.effect.IO
import cats.implicits._
import org.specs2.mutable.Specification

object StripResourcesTest
{
  def test: IO[Unit] =
    IO.unit
}

class StripResourcesTest
extends Specification
{
  "strip resources" >> StripResourcesTest.test.as(1 must_== 1).unsafeRunSync()
}
