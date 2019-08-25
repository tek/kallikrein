package klk

import scala.concurrent.ExecutionContext
// import scala.concurrent.duration.DurationInt

import cats.data.Kleisli
import cats.effect.{ContextShift, IO}
// import cats.implicits._
import org.scalacheck.ForAll
import org.scalacheck.Gen.Parameters
import org.scalacheck.Test.{Parameters => TestParameters}

object RunTest
extends SimpleTest
{
  // def runTest(num: Int): IO[KlkResult[Int, Int]] =
  //   IO.sleep(200.milli).as(assertEqual(0)(num % 3))

  // def one(num: Int): Unit =
  //   test[IO]("something")(runTest(num))

  // List.range(0, 10).foreach(one)

//   def frame1: Boolean =
//     sys.error("boom")

//   test("exception")(IO(frame1))

  implicit def cs: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  test[IO]("prop").forall { l1: List[Int] =>
    IO.delay {
      println(l1.size)
      l1.size < 50
    }
  }
}

object ForAllTest
extends SimpleTest
{
  test[IO]("forall") {
    val f: PropertyTest[IO] = ForAll.noShrink { (a: Int) =>
      ForAll.noShrink { (b: Int) =>
        PropertyTest(Kleisli.pure(PropResult.bool(a != b)))
      }
    }
    val params = ScalacheckParams.cons(TestParameters.default, Parameters.default.withInitialSeed(10L))
    Concurrency.fixedPoolCsWith(10).use(implicit cs => PropertyTest.run(params)(f))
  }
}
