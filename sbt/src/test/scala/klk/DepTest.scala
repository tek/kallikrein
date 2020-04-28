package klk

import scala.concurrent.ExecutionContext

import cats.effect.{ContextShift, IO, Resource}

class DepTest
extends ComposeTest[IO, SbtResources]
{
  def testSuccess: IO[KlkResult[Int]] =
    IO.pure(KlkResult.Single(555, true, KlkResult.Details.NoDetails))

  def testFail: IO[KlkResult[Int]] =
    IO.raiseError(new Exception("boom"))

  implicit def cs: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  // TODO SR builder should be generic.
  // instead of calling builder.test, call test().
  // the constructed data should be parameterized in the thunk, to be evaluated by the SR.
  // composition should work so that only thunks having the resource parameter compile.
  def tests: Suite[IO, Unit, Unit] =
    for {
      _ <- sharedResource(Resource.pure[IO, Int](5))(
        builder =>
          builder.test("five is 4")(five => IO.pure(five == 4)) <+>
          builder.test("five is 5")(five => IO.pure(five == 5))
      )
      a <- test("test 1")(testSuccess)
      _ <- test("test 555")(IO.pure(a == 555))
      _ <- test("test 2")(testFail) <+> test("test 3")(testSuccess)
      _ <- {
        Suite.parallel(test("test 4a")(testSuccess), test("test 4b")(testSuccess)).void <+>
        test("test 5")(testFail).void
      }
      _ <- test("test 7")(testSuccess)
    } yield ()
}
