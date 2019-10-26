package klk

import scala.concurrent.ExecutionContext

import cats.effect.{ContextShift, IO, Resource}

class DepTest
extends ComposeTest[IO, SbtResources]
{
  def testSuccess: IO[Boolean] =
    IO.pure(true)

  def testFail: IO[Boolean] =
    IO.pure(false)

  implicit def cs: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  // TODO SR builder should be generic.
  // instead of calling builder.test, call test().
  // the constructed data should be parameterized in the thunk, to be evaluated by the SR.
  // composition should work so that only thunks having the resource parameter compile.
  def tests: TestAlg[IO, Unit, Unit] =
    for {
      _ <- sharedResource(Resource.pure(5))(
        builder =>
          builder.test("five is 4")(five => IO.pure(five == 4)) <+>
          builder.test("five is 5")(five => IO.pure(five == 5))
      )
      _ <- test("test 1")(testSuccess)
      _ <- test("test 2")(testFail) <+> test("test 3")(testSuccess)
      _ <- TestAlg.parallel(test("test 4a")(testSuccess), test("test 4b")(testSuccess)) <+> test("test 5")(testFail)
      _ <- test("test 7")(testFail)
      _ <- test("test 8")(testSuccess)
    } yield ()
}
