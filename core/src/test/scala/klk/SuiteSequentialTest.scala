package klk

import cats.Eval
import cats.data.NonEmptyList
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class SuiteSequentialTest
extends Specification
{
  def stats(name: String, success: Boolean): TestStats =
    TestStats("test", List(KlkResult.Details.Simple(NonEmptyList.one(name))), success, false, 5L)

  def one(name: String)(continue: Boolean): Suite[Eval, Unit, String] =
    Suite.Suspend(_ => _ =>
        Eval.now(Suite.Output(Suite.Output.Details.Value(name), continue, List(stats(name, continue))))
    )

  def tests: Suite[Eval, Unit, String] =
    Suite.sequential(one("1")(false), one("2")(true), one("3")(true)) >>
    one("4")(true)

  def target: List[TestStats] =
    List(
      stats("1", false),
      stats("2", true),
      stats("3", true),
    )

  def test: Eval[MatchResult[Any]] =
    EvalSuite(tests).run(RunTestResources.cons[Eval](NoopResources))
      .map(_ must_== target)

  "sequential" >> test.value
}
