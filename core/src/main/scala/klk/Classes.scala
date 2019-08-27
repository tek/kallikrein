package klk

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
