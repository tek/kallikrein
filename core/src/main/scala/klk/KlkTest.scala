package klk

case class KlkTest[F[_], A, B](desc: String, thunk: F[KlkResult[A, B]], runner: TestEffect[F])

object KlkTest
{
  def logResult[A, B](reporter: TestReporter[A, B], log: TestLog)(desc: String, result: KlkResult[A, B]): Unit = {
    reporter.result(log)(desc)(result.success)
    if (!result.success) reporter.failure(log)(result.details)
  }

  def run[F[_], A, B](reporter: TestReporter[A, B], log: TestLog): KlkTest[F, A, B] => KlkResult[A, B] = {
    case KlkTest(desc, thunk, runner) =>
      val result = runner.run(thunk)
      logResult(reporter, log)(desc, result)
      result
  }
}
