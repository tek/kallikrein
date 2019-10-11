package klk

final class NoExecutionParams

trait ExecuteThunk[Params, Thunk, Output]
{
  type TestF[A]

  def apply(thunk: Thunk): TestF[Output]
}

trait ExecuteThunk1
{
  type Aux[Params, Thunk, Output, TestF0[_]] = ExecuteThunk[Params, Thunk, Output] { type TestF[A] = TestF0[A] }

  implicit def ExecuteThunk_Any[Output, TestF0[_]]
  : ExecuteThunk.Aux[NoExecutionParams, TestF0[Output], Output, TestF0] =
    new ExecuteThunk[NoExecutionParams, TestF0[Output], Output] {
      type TestF[A] = TestF0[A]
      def apply(thunk: TestF0[Output]): TestF[Output] =
        thunk
    }
}

object ExecuteThunk
extends ExecuteThunk1
{
  implicit def ExecuteThunk_PropertyTestOutput[Trans, Thunk, TestF0[_]]
  (implicit propRun: PropRun.Aux[Thunk, Trans, TestF0])
  : ExecuteThunk.Aux[Trans, Thunk, PropertyTestResult, TestF0] =
    new ExecuteThunk[Trans, Thunk, PropertyTestResult] {
      type TestF[A] = TestF0[A]
      def apply(thunk: Thunk): TestF[PropertyTestResult] =
        PropRun(propRun)(thunk)
    }

  implicit def ExecuteThunk_LawsResult[Thunk, Laws, TestF0[_]]
  (implicit lawsRun: LawsRun.Aux[Thunk, Laws, TestF0])
  : ExecuteThunk.Aux[Laws, Thunk, LawsResult, TestF0] =
    new ExecuteThunk[Laws, Thunk, LawsResult] {
      type TestF[A] = TestF0[A]
      def apply(thunk: Thunk): TestF[LawsResult] =
        LawsRun(lawsRun)(thunk)
    }
}
