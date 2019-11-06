package klk

import scala.concurrent.ExecutionContext

import cats.Id
import cats.effect.{ContextShift, IO}
import org.specs2.mutable.Specification
import shapeless.HNil

class DepTest
extends Specification
{
  private[this] case class Cons()
  extends TestAdder[IO, Id, λ[a => IO[KlkResult[a]]]]
  {
    def apply[A](thunk: Id[IO[KlkResult[A]]]): IO[KlkResult[A]] =
      thunk
  }

  def test: TestBuilder[IO, HNil, Id, λ[a => IO[KlkResult[a]]]] =
    TestBuilder(TestResources.empty)(Cons())

  def testSuccess: IO[KlkResult[Unit]] =
    test(IO.pure(true))

  def testFail: IO[KlkResult[Unit]] =
    test(IO.pure(false))

  def testThunk(desc: String)(thunk: IO[KlkResult[Unit]]): KlkTest[IO, Unit, Unit] =
    KlkTest.plain(desc)(thunk)

  def cons(desc: String)(thunk: IO[KlkResult[Unit]]): Suite[IO, Unit, Unit] =
    Suite.single(testThunk(desc)(thunk))

  def tests: Suite[IO, Unit, Unit] =
    for {
      _ <- cons("test 1")(testSuccess)
      _ <- cons("test 2")(testFail) <+> cons("test 3")(testSuccess)
      _ <- cons("test 4")(testSuccess) <+> cons("test 5")(testFail)
      _ <- cons("test 6")(testFail)
      _ <- cons("test 7")(testSuccess)
    } yield ()

  implicit def cs: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  def stat(name: String, success: Boolean): (String, KlkResult[Unit]) =
    (name, KlkResult.Single((), success, KlkResult.Details.NoDetails))

  val target: List[(String, KlkResult[Unit])] =
    List(
      stat("test 1", true),
      stat("test 2", false),
      stat("test 3", true),
      stat("test 4", true),
      stat("test 6", false),
    )

  "dependent tests" >> {
    EvalSuite(tests)
      .run(RunTestResources.cons(NoopResources))
      .map(_.map { case TestStats(desc, _, success, _, _) => stat(desc, success) })
      .map(_.must_==(target))
      .unsafeRunSync()
  }
}
