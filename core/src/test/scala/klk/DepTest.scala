package klk

import scala.concurrent.ExecutionContext

import cats.Id
import cats.effect.{ContextShift, IO}
import org.specs2.mutable.Specification
import shapeless.HNil

class DepTest
extends Specification
{
  def test: TestBuilder[IO, HNil, Id, IO[KlkResult]] =
    TestBuilder(TestResources.empty)(identity)

  def testSuccess: IO[KlkResult] =
    test(IO.pure(true))

  def testFail: IO[KlkResult] =
    test(IO.pure(false))

  def testThunk(desc: String)(thunk: IO[KlkResult]): KlkTest[IO, Unit] =
    KlkTest.plain(desc)(thunk)

  def cons(desc: String)(thunk: IO[KlkResult]): TestAlg[IO, Unit, Unit] =
    TestAlg.single(testThunk(desc)(thunk))

  def tests: TestAlg[IO, Unit, Unit] =
    for {
      _ <- cons("test 1")(testSuccess)
      _ <- cons("test 2")(testFail) <+> cons("test 3")(testSuccess)
      _ <- cons("test 4")(testSuccess) <+> cons("test 5")(testFail)
      _ <- cons("test 6")(testFail)
      _ <- cons("test 7")(testSuccess)
    } yield ()

  implicit def cs: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  "dependent tests" >> {
    println(RunTestAlg.run(tests).run(RunTestResources.cons(NoopResources)).unsafeRunSync())
    1 === 1
  }
}
