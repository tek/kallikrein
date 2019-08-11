package klk

import scala.concurrent.duration.DurationInt

import cats.implicits._
import cats.effect.IO

object RunTest
extends SimpleTest
{
  def runTest(num: Int): IO[TestResult[Boolean, Boolean]] =
    IO.sleep(200.milli).as(assert(s"test number $num")(num % 3 == 0))

  def one(num: Int): Unit =
    test("something")(runTest(num))

  List.range(0, 10).foreach(one)

  def frame1: Boolean =
    sys.error("boom")

  test("exception")(IO(frame1))
}
