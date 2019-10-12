package klk

import scala.concurrent.ExecutionContext

import cats.{Applicative, Functor}
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

case class SbtTestLog(loggers: Array[Logger])

object SbtTestLog
{
  def sync[F[_]: Sync](log: SbtTestLog)(f: Logger => String => Unit): List[String] => F[Unit] =
    lines =>
      log.loggers.toList.traverse_(logger => lines.traverse_(line => Sync[F].delay(f(logger)(line))))

  def unsafe(log: SbtTestLog)(f: Logger => String => Unit)(lines: List[String]): Unit =
      log.loggers.toList.foreach(logger => lines.foreach(line => f(logger)(line)))
}

trait TestReporter[F[_]]
{
  def result: String => Boolean => F[Unit]
  def failure: KlkResult.Details => F[Unit]
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

  def report[F[_]: Applicative]
  (reporter: TestReporter[F])
  (desc: String)
  (result: KlkResult)
  : F[Unit] =
    reporter.result(desc)(KlkResult.successful(result)) *>
    KlkResult.failures(result).traverse_(reporter.failure)


  def sbt[F[_]: Sync](log: SbtTestLog): TestReporter[F] =
    SbtTestReporter(log)

  def noop[F[_]: Applicative]: TestReporter[F] =
    NoopTestReporter()
}

case class SbtTestReporter[F[_]: Sync](log: SbtTestLog)
extends TestReporter[F]
{
  def result: String => Boolean => F[Unit] =
    desc => SbtTestLog.sync[F](log)(_.info).compose(TestReporter.formatResult(desc))

  def failure: KlkResult.Details => F[Unit] =
    SbtTestLog.sync[F](log)(_.error).compose(Indent(2)).compose(TestReporter.formatFailure)
}

case class NoopTestReporter[F[_]: Applicative]()
extends TestReporter[F]
{
  def result: String => Boolean => F[Unit] =
    _ => _ => ().pure[F]

  def failure: KlkResult.Details => F[Unit] =
    _ => ().pure[F]
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

  implicit def TestResult_Either[A, B]
  (implicit inner: TestResult[B])
  : TestResult[Either[A, B]] =
    new TestResult[Either[A, B]] {
      def apply(output: Either[A, B]): KlkResult =
        output.map(inner(_)).valueOr(a => KlkResult.simpleFailure(List(a.toString)))
    }
}

trait Compile[F[_], G[_], A]
{
  def apply(fa: F[A]): G[KlkResult]
}

object Compile
{
  implicit def Compile_F_F[F[_]: Functor, A]
  (implicit result: TestResult[A])
  : Compile[F, F, A] =
    new Compile[F, F, A] {
      def apply(fa: F[A]): F[KlkResult] =
        fa.map(result(_))
    }

  implicit def Compile_EitherT_F[F[_]: Functor, G[_], E, A]
  (implicit inner: Compile[F, G, Either[E, A]])
  : Compile[EitherT[F, E, ?], G, A] =
    new Compile[EitherT[F, E, ?], G, A] {
      def apply(fa: EitherT[F, E, A]): G[KlkResult] =
        inner(fa.value)
    }
}
