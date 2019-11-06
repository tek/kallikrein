package klk

import cats.Eval
import cats.data.NonEmptyList
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class SuiteUnlessTest
extends Specification
{
  def stats(name: String, success: Boolean): TestStats =
    TestStats("test", List(KlkResult.Details.Simple(NonEmptyList.one(name))), success, false, 5L)

  def one(name: String)(continue: Boolean): Suite[Eval, Unit, String] =
    Suite.Suspend(_ => _ =>
        Eval.now(Suite.Output(Suite.Output.Details.Value(name), continue, List(stats(name, continue))))
    )

  def tests: Suite[Eval, Unit, String] =
    one("0")(true) >>
    (one("1")(false) <+> (one("2")(true) <+> one("3")(true))) >>
    one("4")(true)

  def target: List[TestStats] =
    List(
      TestStats("test", List(KlkResult.Details.Simple(NonEmptyList.one("0"))), true, false, 5L),
      TestStats("test", List(KlkResult.Details.Simple(NonEmptyList.one("1"))), false, true, 5L),
      TestStats("test", List(KlkResult.Details.Simple(NonEmptyList.one("2"))), true, false, 5L),
      TestStats("test", List(KlkResult.Details.Simple(NonEmptyList.one("4"))), true, false, 5L),
    )

  def test: Eval[MatchResult[Any]] =
    EvalSuite(tests).run(RunTestResources.cons[Eval](NoopResources))
      .map(_ must_== target)

  "unless" >> test.value
}
