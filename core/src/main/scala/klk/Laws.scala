package klk

case class LawsResult()

object LawsResult
{
  implicit def TestResult_LawsResult: TestResult[LawsResult] =
    new TestResult[LawsResult] {
      def apply(output: LawsResult): KlkResult =
        KlkResult.bool(false)
    }
}

trait FunctorialLaws[Class[_[A]], Subject[_]]

trait LawsRun[Thunk, Laws]
{
  type TestF[A]

}

object LawsRun
{
  type Aux[Thunk, Laws, TestF0[_]] = LawsRun[Thunk, Laws] { type TestF[A] = TestF0[A] }

  implicit def LawsRun_Any[F[_], Laws]: Aux[F[LawsResult], Laws, F] =
    new LawsRun[F[LawsResult], Laws] {
      type TestF[A] = F[A]
    }

  def apply[Thunk, Laws](lawsRun: LawsRun[Thunk, Laws])(thunk: Thunk): lawsRun.TestF[LawsResult] =
    ???
}
