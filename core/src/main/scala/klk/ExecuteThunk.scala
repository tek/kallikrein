package klk

trait ExecuteThunk[Thunk, Output]
{
  type TestF[A]

  def apply(thunk: Thunk): TestF[Output]
}

trait ExecuteThunk1
{
  type Aux[Thunk, Output, TestF0[_]] = ExecuteThunk[Thunk, Output] { type TestF[A] = TestF0[A] }

  implicit def ExecuteThunk_Any[Output, TestF0[_]]
  : ExecuteThunk.Aux[TestF0[Output], Output, TestF0] =
    new ExecuteThunk[TestF0[Output], Output] {
      type TestF[A] = TestF0[A]
      def apply(thunk: TestF0[Output]): TestF[Output] =
        thunk
    }
}

object ExecuteThunk
extends ExecuteThunk1
{
  implicit def ExecuteThunk_PropertyTestOutput[Thunk, Trans, TestF0[_]]
  (implicit propRun: PropRun.Aux[Thunk, Trans, TestF0])
  : ExecuteThunk.Aux[Thunk, PropertyTestOutput[Trans], TestF0] =
    new ExecuteThunk[Thunk, PropertyTestOutput[Trans]] {
      type TestF[A] = TestF0[A]
      def apply(thunk: Thunk): TestF[PropertyTestOutput[Trans]] =
        PropRun(propRun)(thunk)
    }
}
