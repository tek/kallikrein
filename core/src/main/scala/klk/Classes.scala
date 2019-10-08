package klk

import scala.concurrent.ExecutionContext

import cats.Functor
import cats.data.EitherT
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
  def failure[F[_]: Sync](log: TestLog): KlkResult.Details => F[Unit]
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

  def formatFailure: KlkResult.Details => List[String] = {
    case KlkResult.Details.NoDetails() =>
      List("test failed")
    case KlkResult.Details.Simple(info) =>
      info
    case KlkResult.Details.Complex(desc, target, actual) =>
      desc ::: Indent(2)(List(s"target: ${target.toString.green}", s"actual: ${actual.toString.magenta}"))
    case KlkResult.Details.Fatal(error) =>
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
    reporter.result[F](log).apply(desc)(KlkResult.successful(result)) *>
    KlkResult.failures(result).traverse_(reporter.failure[F](log))
  }


  def stdout: TestReporter =
    new TestReporter {
      def result[F[_]: Sync](log: TestLog): String => Boolean => F[Unit] =
        desc => (log.info[F] _).compose(formatResult(desc))

      def failure[F[_]: Sync](log: TestLog): KlkResult.Details => F[Unit] =
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

trait TestResult[Output]
{
  def apply(output: Output): KlkResult
}

object TestResult
{
  implicit def TestResult_KlkResult: TestResult[KlkResult] =
    new TestResult[KlkResult] {
      def apply(output: KlkResult): KlkResult =
        output
    }

  implicit def TestResult_Boolean: TestResult[Boolean] =
    new TestResult[Boolean] {
      def apply(output: Boolean): KlkResult =
        KlkResult(output)(KlkResult.Details.NoDetails())
    }

  implicit def TestResult_PropertyTestOutput[Trans]: TestResult[PropertyTestOutput[Trans]] =
    new TestResult[PropertyTestOutput[Trans]] {
      def apply(output: PropertyTestOutput[Trans]): KlkResult = {
        KlkResult(output.result.success)(PropertyTestResult.resultDetails(output.result))
      }
    }
}

trait Compile[F[_], G[_]]
{
  def apply[A](fa: F[A]): G[Either[KlkResult.Details, A]]
}

object Compile
{
  implicit def Compile_F_F[F[_]: Functor]: Compile[F, F] =
    new Compile[F, F] {
      def apply[A](fa: F[A]): F[Either[KlkResult.Details, A]] =
        fa.map(Right(_))
    }

  implicit def Compile_EitherT_F[E]: Compile[EitherT[IO, E, ?], IO] =
    new Compile[EitherT[IO, E, ?], IO] {
      def apply[A](fa: EitherT[IO, E, A]): IO[Either[KlkResult.Details, A]] =
        fa.leftMap(a => KlkResult.Details.Simple(List(a.toString))).value
    }
}
