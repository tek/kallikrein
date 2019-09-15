package klk

import cats.Functor
import cats.effect.{Concurrent, IO, Resource, Sync}
import cats.implicits._
import sbt.testing.Logger

import StringColor._
import StringColors.color

case class TestLog(loggers: Array[Logger])
{
  def info[F[_]: Sync](lines: List[String]): F[Unit] =
    loggers.toList.traverse_(logger => lines.traverse_(line => Sync[F].delay(logger.info(line))))
}

trait TestReporter
{
  def result[F[_]: Sync](log: TestLog): String => Boolean => F[Unit]
  def failure[F[_]: Sync, E, A](log: TestLog): KlkResultDetails[E, A] => F[Unit]
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

  def formatFailure[E, A]: KlkResultDetails[E, A] => List[String] = {
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

  def report[F[_]: Sync, E, A]
  (reporter: TestReporter, log: TestLog)
  (desc: String)
  (result: KlkResult[E, A])
  : F[Unit] =
    reporter.result[F](log).apply(desc)(result.success) *>
    reporter.failure[F, E, A](log).apply(result.details).unlessA(result.success)


  def stdout: TestReporter =
    new TestReporter {
      def result[F[_]: Sync](log: TestLog): String => Boolean => F[Unit] =
        desc => (log.info[F] _).compose(formatResult(desc))

      def failure[F[_]: Sync, E, A](log: TestLog): KlkResultDetails[E, A] => F[Unit] =
        (log.info[F] _).compose(indent(2)).compose(formatFailure)
    }
}

trait TestEffect[F[_]]
{
  def run[A](thunk: F[A]): A
  def concurrentPool: Resource[F, Concurrent[F]]
  def sync: Sync[F]
}

// TODO change back to KlkResult
object TestEffect
{
  implicit def io: TestEffect[IO] =
    new TestEffect[IO] {
      def run[A](thunk: IO[A]): A =
        thunk
          // .recover { case NonFatal(a) => KlkResult(false, KlkResultDetails.Fatal[E, A](a)) }
          .unsafeRunSync

      def concurrentPool: Resource[IO, Concurrent[IO]] =
        Concurrency.fixedPoolCs.map(IO.ioConcurrentEffect(_))

      def sync: Sync[IO] =
        Sync[IO]
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
        output.map(result => KlkResult(result.success, PropertyTestResult.resultDetails(result)))
      }
    }
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
