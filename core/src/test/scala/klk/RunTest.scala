package klk

import scala.concurrent.duration.DurationInt

import cats.implicits._
import cats.effect.IO

object RunTest
extends SimpleTest
{
  def runTest(num: Int): IO[TestResult[Int, Int]] =
    IO.sleep(200.milli).as(assertEqual(0)(num % 3))

  def one(num: Int): Unit =
    test("something")(runTest(num))

  List.range(0, 10).foreach(one)

  def frame1: Boolean =
    sys.error("boom")

  test("exception")(IO(frame1))
}
