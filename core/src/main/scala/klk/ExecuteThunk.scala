package klk

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

  implicit def ExecuteThunk_LawsResult[Thunk, Laws, TestF0[_]]
  (implicit lawsRun: LawsRun.Aux[Thunk, Laws, TestF0])
  : ExecuteThunk.Aux[Laws, Thunk, TestF0, LawsResult] =
    new ExecuteThunk[Laws, Thunk] {
      type TestF[A] = TestF0[A]
      type Output = LawsResult
      def apply(thunk: Thunk): TestF[LawsResult] =
        LawsRun(lawsRun)(thunk)
    }
}
