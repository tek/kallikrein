package klk

import cats.Monad
import cats.implicits._
import org.scalacheck.Prop
import org.typelevel.discipline.Laws

final class NoExecutionParams

trait ExecuteThunk[Params, Thunk]
{
  type TestF[A]
  type Output

  def apply(thunk: Thunk): TestF[Output]
}

trait ExecuteThunk1
{
  type Aux[Params, Thunk, TestF0[_], Output0] =
    ExecuteThunk[Params, Thunk] {
      type TestF[A] = TestF0[A]
      type Output = Output0
    }

  implicit def ExecuteThunk_Any[TestF0[_], Output0]
  : ExecuteThunk.Aux[NoExecutionParams, TestF0[Output0], TestF0, Output0] =
    new ExecuteThunk[NoExecutionParams, TestF0[Output0]] {
      type TestF[A] = TestF0[A]
      type Output = Output0
      def apply(thunk: TestF0[Output]): TestF[Output] =
        thunk
    }
}

object ExecuteThunk
extends ExecuteThunk1
{
  implicit def ExecuteThunk_PropertyTestOutput[Trans, Thunk, TestF0[_]]
  (implicit propRun: PropRun.Aux[Thunk, Trans, TestF0])
  : ExecuteThunk.Aux[Trans, Thunk, TestF0, PropertyTestResult] =
    new ExecuteThunk[Trans, Thunk] {
      type TestF[A] = TestF0[A]
      type Output = PropertyTestResult
      def apply(thunk: Thunk): TestF[PropertyTestResult] =
        PropRun(propRun)(thunk)
    }

  implicit def ExecuteThunk_LawsResult[TestF0[_]: Monad, L <: Laws]
  (implicit propRun: PropRun.Aux[TestF0[Prop], LawsParams, TestF0])
  : ExecuteThunk.Aux[LawsParams, TestF0[L#RuleSet], TestF0, LawsResult] =
    new ExecuteThunk[LawsParams, TestF0[L#RuleSet]] {
      type TestF[A] = TestF0[A]
      type Output = LawsResult
      def apply(thunk: TestF[L#RuleSet]): TestF[LawsResult] =
        thunk.flatMap(LawsTest(propRun))
    }
}
