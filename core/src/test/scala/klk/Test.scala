package klk

import scala.concurrent.duration.DurationInt

import cats.Functor
import cats.data.{EitherT, Kleisli}
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

  test("EitherT")(EitherT.right[Unit](IO.pure(1 == 1)))
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
    PropertyTest.run(ConsConcurrent.io)(params)(f)
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

  val testResource: Resource[IO, Int] = Resource.pure(4)

  eightySix
    .test("shared resource shrink")
    .resource(testResource)
    .forall((i: Int) => (j: Int) => (k: Int) => IO.pure(i + j < k))
}

class ResTest
extends IOTest
{
  val res1: Resource[IO, Int] = Resource.pure(1)

  val res2: Resource[IO, Int] = Resource.pure(1)

  test("resource").resource(res1).resource(res2)((i: Int) => (j: Int) => IO.pure(i == j))
}

case class Funky[A](a: A)

object Funky
{
  implicit def Functor_Funky: Functor[Funky] =
    new Functor[Funky] {
      def map[A, B](fa: Funky[A])(f: A => B): Funky[B] =
        Funky(f(fa.a))
    }
}

// class LawsTest
// extends IOTest
// {
//   test("laws").laws[Functor, Funky] {
//     IO(LawsResult())
//   }
// }
