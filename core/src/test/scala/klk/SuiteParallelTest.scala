package klk

import cats.data.NonEmptyList
import cats.effect.{ContextShift, IO}
import cats.effect.concurrent.Ref
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class SuiteParallelTest
extends Specification
{
  def stats(name: String, success: Boolean): TestStats =
    TestStats("test", List(KlkResult.Details.Simple(NonEmptyList.one(name))), success, false, 5L)

  def wait
  (counter: Ref[IO, Int])
  (target: Int)
  : IO[Unit] =
    counter.get.flatMap { i => if (i < target) wait(counter)(target) else IO.unit }

  def one
  (counter: Ref[IO, Int])
  (target: Int)
  (continue: Boolean)
  : Suite[IO, Unit, Int] =
    Suite.Suspend(
      _ => _ =>
        wait(counter)(target) *>
        counter.update(_ + 1) *>
        IO.pure(Suite.Output(Suite.Output.Details.Value(target), continue, List(stats(target.toString, continue))))
    )

  def tests(counter: Ref[IO, Int])(implicit cs: ContextShift[IO]): Suite[IO, Unit, Int] =
    Suite.parallel(one(counter)(3)(false), one(counter)(2)(true), one(counter)(1)(true)) >>
    one(counter)(4)(true)

  def target: List[TestStats] =
    List(
      stats("3", false),
      stats("2", true),
      stats("1", true),
    )

  def test: IO[MatchResult[Any]] =
    Concurrency.fixedPoolCs.use(
      implicit cs =>
        for {
          counter <- Ref.of[IO, Int](1)
          result <- EvalSuite(tests(counter)).run(RunTestResources.cons[IO](NoopResources))
        } yield result must_== target
    )

  "parallel" >> test.unsafeRunSync()
}
