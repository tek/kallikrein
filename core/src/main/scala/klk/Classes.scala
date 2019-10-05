package klk

import scala.concurrent.ExecutionContext

import cats.Functor
import cats.effect.{Concurrent, IO, Sync}
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
      .takeWhile(a => !a.getClassName.startsWith("klk.Compute"))
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

trait Compute[F[_]]
{
  def run[A](thunk: F[A]): A
}

object Compute
{
  implicit def io: Compute[IO] =
    new Compute[IO] {
      def run[A](thunk: IO[A]): A =
        thunk
          .unsafeRunSync
    }
}

trait ConsConcurrent[F[_]]
{
  def pool(ec: ExecutionContext): Concurrent[F]
}

object ConsConcurrent
{
  implicit def io: ConsConcurrent[IO] =
    new ConsConcurrent[IO] {
      def pool(ec: ExecutionContext): Concurrent[IO] =
        IO.ioConcurrentEffect(IO.contextShift(ec))
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
