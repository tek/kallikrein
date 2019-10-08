package klk

import scala.concurrent.duration.DurationInt

import cats.data.Kleisli
import cats.effect.{IO, Resource}
import cats.implicits._
import org.scalacheck.ForAllNoShrink
import org.scalacheck.Gen.Parameters
import org.scalacheck.Test.{Parameters => TestParameters}

class BasicTest
extends IOTest
{
  def runTest(num: Int): IO[KlkResult] =
    IO.sleep(200.milli).as(assertEqual(0)(num % 3))

  def one(num: Int): Unit =
    test("something")(runTest(num))

  List.range(0, 10).foreach(one)

  def frame1: Boolean =
    sys.error("boom")

  test("exception")(IO(frame1))
}

class ForAllTest
extends IOTest
{
  test("forall") {
    val f: PropertyTest[IO] = ForAllNoShrink { (a: Int) =>
      ForAllNoShrink { (b: Int) =>
        PropertyTest(Kleisli.pure(PropResult.bool(a != b)))
      }
    }
    val params = ScalacheckParams.cons(TestParameters.default, Parameters.default.withInitialSeed(10L))
    PropertyTest.run(ConsConcurrent.io)(params)(f).map(PropertyTestOutput(_))
  }
}

class PropTest
extends IOTest
{
  test("are all lists of integers shorter than 5 elements?").forallNoShrink((l: List[Int]) => IO(l.size < 5))
}

class PropShrinkTest
extends IOTest
{
  test("shrink").forall((i: Int) => IO.pure(i > 0))
}

class SharedResTest
extends IOTest
{
  def eightySix: SharedResource[IO, Int] =
    sharedResource(Resource.pure(86))

  eightySix.test("shared resource 1").apply(i => IO.pure(i == 86))

  eightySix.test("shared resource 2").apply(i => IO.pure(i == 68))
}

class ResTest
extends IOTest
{
  val res1: Resource[IO, Int] = Resource.pure(1)

  val res2: Resource[IO, Int] = Resource.pure(1)

  test("resource").resource(res1).resource(res2)((i: Int) => (j: Int) => IO.pure(i == j))
}