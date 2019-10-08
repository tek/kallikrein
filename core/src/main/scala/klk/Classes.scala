package klk

import scala.concurrent.ExecutionContext

import cats.Functor
import cats.arrow.FunctionK
import cats.effect.{Concurrent, IO, Sync}
import cats.implicits._
import sbt.testing.Logger

import StringColor._
import StringColors.color

object Indent
{
  def apply(spaces: Int)(lines: List[String]): List[String] =
    lines.map(a => s"${" " * spaces}$a")

}

case class TestLog(loggers: Array[Logger])
{
  def info[F[_]: Sync](lines: List[String]): F[Unit] =
    loggers.toList.traverse_(logger => lines.traverse_(line => Sync[F].delay(logger.info(line))))
}

trait TestReporter
{
  def result[F[_]: Sync](log: TestLog): String => Boolean => F[Unit]
  def failure[F[_]: Sync](log: TestLog): KlkResultDetails => F[Unit]
}

object TestReporter
{
  def packageFrameFilter: List[String] =
    List(
      "cats.effect",
      "scala.runtime",
      "scala.concurrent",
    )

  def sanitizeStacktrace(trace: List[StackTraceElement]): List[String] =
    trace
      .takeWhile(a => !a.getClassName.startsWith("klk.Compute"))
      .reverse
      .dropWhile(a => packageFrameFilter.exists(a.getClassName.startsWith))
      .reverse
      .map(_.toString)

  def formatFailure: KlkResultDetails => List[String] = {
    case KlkResultDetails.NoDetails() =>
      List("test failed")
    case KlkResultDetails.Simple(info) =>
      info
    case KlkResultDetails.Complex(desc, target, actual) =>
      desc ::: Indent(2)(List(s"target: ${target.toString.green}", s"actual: ${actual.toString.magenta}"))
    case KlkResultDetails.Fatal(error) =>
      s"${"test threw".blue} ${error.toString.magenta}" :: Indent(2)(sanitizeStacktrace(error.getStackTrace.toList))
  }

  def successSymbol: Boolean => String = {
    case false => "✘".red
    case true => "✔".green
  }

  def formatResult(desc: String)(success: Boolean): List[String] =
    List(s"${successSymbol(success)} $desc")

  def report[F[_]: Sync]
  (reporter: TestReporter, log: TestLog)
  (desc: String)
  (result: KlkResult)
  : F[Unit] = {
    reporter.result[F](log).apply(desc)(KlkResult.success(result)) *>
    KlkResult.failures(result).traverse_(reporter.failure[F](log))
  }


  def stdout: TestReporter =
    new TestReporter {
      def result[F[_]: Sync](log: TestLog): String => Boolean => F[Unit] =
        desc => (log.info[F] _).compose(formatResult(desc))

      def failure[F[_]: Sync](log: TestLog): KlkResultDetails => F[Unit] =
        (log.info[F] _).compose(Indent(2)).compose(formatFailure)
    }
}

trait Compute[F[_]]
{
  def run[A](thunk: F[A]): A
}

object Compute
{
  def apply[F[_]](implicit instance: Compute[F]): Compute[F] =
    instance

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

trait TestResult[F[_], Output]
{
  def handle(output: F[Output]): F[KlkResult]
}

object TestResult
{
  implicit def TestResult_KlkResult[F[_]]: TestResult[F, KlkResult] =
    new TestResult[F, KlkResult] {
      def handle(output: F[KlkResult]): F[KlkResult] =
        output
    }

  implicit def TestResult_Boolean[F[_]: Functor]: TestResult[F, Boolean] =
    new TestResult[F, Boolean] {
      def handle(output: F[Boolean]): F[KlkResult] =
        output.map(a => KlkResult(a, KlkResultDetails.NoDetails()))
    }

  implicit def TestResult_PropertyTestOutput[F[_]: Functor, Trans]: TestResult[F, PropertyTestOutput[Trans]] =
    new TestResult[F, PropertyTestOutput[Trans]] {
      def handle(output: F[PropertyTestOutput[Trans]]): F[KlkResult] = {
        output.map(result => KlkResult(result.result.success, PropertyTestResult.resultDetails(result.result)))
      }
    }
}

trait Compile[F[_], G[_]]
extends FunctionK[F, G]

object Compile
{
  implicit def Compile_IO_IO: Compile[IO, IO] =
    new Compile[IO, IO] {
      def apply[A](fa: IO[A]): IO[A] =
        fa
    }
}
