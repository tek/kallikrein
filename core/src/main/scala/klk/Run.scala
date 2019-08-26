package klk

import scala.collection.mutable
import scala.util.control.NonFatal

import cats.Functor
import cats.effect.{Concurrent, IO, Resource, Sync}
import cats.implicits._
import sbt.testing.Logger

import StringColor._
import StringColors.color

case class TestLog(loggers: Array[Logger])
{
  def info(lines: List[String]): Unit =
    loggers.foreach(l => lines.foreach(l.info))
}

trait TestReporter[A, B]
{
  def result(log: TestLog): String => Boolean => Unit
  def failure(log: TestLog): KlkResultDetails[A, B] => Unit
}

object TestReporter
{
  def indent(spaces: Int)(lines: List[String]): List[String] =
    lines.map(a => s"${" " * spaces}$a")

  def sanitizeStacktrace(trace: List[StackTraceElement]): List[String] =
    trace
      .takeWhile(a => !a.getClassName.startsWith("klk.TestEffect"))
      .reverse
      .dropWhile(a => a.getClassName.startsWith("cats.effect"))
      .dropWhile(a => a.getClassName.startsWith("scala.runtime"))
      .reverse
      .map(_.toString)

  def formatFailure[A, B]: KlkResultDetails[A, B] => List[String] = {
    case KlkResultDetails.NoDetails() =>
      List("test failed")
    case KlkResultDetails.Simple(info) =>
      info
    case KlkResultDetails.Complex(desc, target, actual) =>
      desc ::: indent(2)(List(s"target: ${target.toString.green}", s"actual: ${actual.toString.magenta}"))
    case KlkResultDetails.Fatal(error) =>
      s"${"test threw".blue} ${error.toString.magenta}" :: indent(2)(sanitizeStacktrace(error.getStackTrace.toList))
  }

  def successSymbol: Boolean => String = {
    case false => "✘".red
    case true => "✔".green
  }

  def formatResult(desc: String)(success: Boolean): List[String] =
    List(s"${successSymbol(success)} $desc")

  def stdout[A, B]: TestReporter[A, B] =
    new TestReporter[A, B] {
      def result(log: TestLog): String => Boolean => Unit =
        desc => (log.info _).compose(formatResult(desc))

      def failure(log: TestLog): KlkResultDetails[A, B] => Unit =
        (log.info _).compose(indent(2)).compose(formatFailure)
    }
}

trait TestEffect[F[_]]
{
  def run[A, B](thunk: F[KlkResult[A, B]]): KlkResult[A, B]
  def concurrentPool: Resource[F, Concurrent[F]]
  def sync: Sync[F]
}

object TestEffect
{
  implicit def io: TestEffect[IO] =
    new TestEffect[IO] {
      def run[A, B](thunk: IO[KlkResult[A, B]]): KlkResult[A, B] =
        thunk
          .recover { case NonFatal(a) => KlkResult(false, KlkResultDetails.Fatal[A, B](a)) }
          .unsafeRunSync

      def concurrentPool: Resource[IO, Concurrent[IO]] =
        Concurrency.fixedPoolCs.map(IO.ioConcurrentEffect(_))

      def sync: Sync[IO] =
        Sync[IO]
    }
}

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

trait TestResult[F[_], Output, Expected, Actual]
{
  def handle(output: F[Output]): F[KlkResult[Expected, Actual]]
}

object TestResult
{
  implicit def TestResult_KlkResult[F[_], E, A]: TestResult[F, KlkResult[E, A], E, A] =
    new TestResult[F, KlkResult[E, A], E, A] {
      def handle(output: F[KlkResult[E, A]]): F[KlkResult[E, A]] =
        output
    }

  implicit def TestResult_Boolean[F[_]: Functor]: TestResult[F, Boolean, Boolean, Boolean] =
    new TestResult[F, Boolean, Boolean, Boolean] {
      def handle(output: F[Boolean]): F[KlkResult[Boolean, Boolean]] =
        output.map(a => KlkResult(a, KlkResultDetails.NoDetails()))
    }

  implicit def TestResult_PropertyTestResult[F[_]: Functor]: TestResult[F, PropertyTestResult, Boolean, Boolean] =
    new TestResult[F, PropertyTestResult, Boolean, Boolean] {
      def handle(output: F[PropertyTestResult]): F[KlkResult[Boolean, Boolean]] = {
        output.map(result => KlkResult(result.success, KlkResultDetails.NoDetails()))
      }
    }
}

trait TestParams[F[_], Thunk, Params, Output]
{
  def func(thunk: Thunk): Params => F[Output]
}

case class TestFunction[F[_], Output](thunk: F[Output])

object TestFunction
{
  def execute[F[_], Output, Expected, Actual]: TestFunction[F, Output] => F[Output] = {
    case TestFunction(f) =>
      f
  }
}

trait TestInput[F[_], Thunk]
{
  type Output

  def bracket(f: Thunk): TestFunction[F, Output]
}

object TestInput
{
  type Aux[F[_], Thunk, Output0] = TestInput[F, Thunk] { type Output = Output0 }

  implicit def TestInput_Any[F[_], Output0]: TestInput.Aux[F, F[Output0], Output0] =
    new TestInput[F, F[Output0]] {
      type Output = Output0
      def bracket(f: F[Output]): TestFunction[F, Output] = {
        TestFunction(f)
      }
    }
}

trait Test
{
  def reporter[A, B]: TestReporter[A, B]

  val tests: mutable.Buffer[KlkTest[F, A, B] forSome { type F[_]; type A; type B }] =
    mutable.Buffer.empty

  class TestPartiallyApplied[F[_]](desc: String) {
    def apply[Thunk, Params, Output, Expected, Actual]
    (thunk: Thunk)
    (
      implicit
      input: TestInput.Aux[F, Thunk, Output],
      result: TestResult[F, Output, Expected, Actual],
      effect: TestEffect[F],
    )
    : Unit =
      tests += Test[F, Thunk, Params, Output, Expected, Actual](input, result, effect)(desc)(thunk)

    def forall[Thunk, Params, Expected, Actual]
    (thunk: Thunk)
    (
      implicit
      input: PropGen[F, Thunk],
      result: TestResult[F, PropertyTestResult, Expected, Actual],
      effect: TestEffect[F],
    )
    : Unit =
      tests += Test.forall[F, Thunk, Params, PropertyTestResult, Expected, Actual](input, result, effect)(desc)(thunk)
  }

  def test[F[_]]
  (desc: String)
  : TestPartiallyApplied[F] =
    new TestPartiallyApplied(desc)

  // import scala.language.implicitConversions

  // implicit def test0[T, A, B](t: => T)(implicit output: ConstructTest[IO, () => T, A, B]): () => T =
  //   () => output match { case _ => t }
}

object Test
{
  def apply[F[_], T, P, O, E, R]
  (input: TestInput.Aux[F, T, O], result: TestResult[F, O, E, R], effect: TestEffect[F])
  (desc: String)
  (thunk: T)
  : KlkTest[F, E, R] =
    KlkTest(desc, result.handle(TestFunction.execute(input.bracket(thunk))), effect)

  def forall[F[_], T, P, O, E, R]
  (input: PropGen[F, T], result: TestResult[F, PropertyTestResult, E, R], effect: TestEffect[F])
  (desc: String)
  (thunk: T)
  : KlkTest[F, E, R] =
    KlkTest(
      desc,
      result.handle(TestFunction.execute(PropGen(effect.concurrentPool)(thunk)(effect.sync, input))),
      effect,
    )
}
