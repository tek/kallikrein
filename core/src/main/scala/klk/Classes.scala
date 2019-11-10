package klk

import scala.concurrent.ExecutionContext

import cats.{Applicative, Comonad, Functor}
import cats.data.{EitherT, NonEmptyList}
import cats.effect.{Concurrent, IO}
import fs2.Stream

import StringColor._
import StringColors.color

object Indent
{
  def apply[T[_]: Functor](spaces: Int)(lines: T[String]): T[String] =
    lines.map(a => s"${" " * spaces}$a")

}

trait TestReporter[F[_]]
{
  def result: String => Boolean => F[Unit]
  def failure: KlkResult.Details => F[Unit]
  def fatal: Throwable => F[Unit]
}

object TestReporter
{
  def packageFrameFilter: List[String] =
    List(
      "cats.effect",
      "scala.runtime",
      "scala.concurrent",
      "java",
    )

  def sanitizeStacktrace(trace: List[StackTraceElement]): List[String] =
    trace
      .takeWhile(a => !a.getClassName.startsWith("klk.Compute"))
      .reverse
      .dropWhile(a => packageFrameFilter.exists(a.getClassName.startsWith))
      .reverse
      .map(_.toString)

  def formatFailure: KlkResult.Details => NonEmptyList[String] = {
    case KlkResult.Details.NoDetails =>
      NonEmptyList.one("test failed")
    case KlkResult.Details.Simple(info) =>
      info
    case KlkResult.Details.Complex(desc, target, actual) =>
      desc ::: Indent(2)(NonEmptyList.of(s"target: ${target.toString.green}", s"actual: ${actual.toString.magenta}"))
  }

  def formatFatal: Throwable => NonEmptyList[String] =
    error =>
      NonEmptyList(
        s"${"test threw".blue} ${error.toString.magenta}",
        Indent(2)(sanitizeStacktrace(error.getStackTrace.toList)),
      )

  def successSymbol: Boolean => String = {
    case false => "✘".red
    case true => "✔".green
  }

  def formatResult(desc: String)(success: Boolean): NonEmptyList[String] =
    NonEmptyList.one(s"${successSymbol(success)} $desc")

  def report[F[_]: Applicative, A]
  (reporter: TestReporter[F])
  (desc: String)
  (result: KlkResult[A])
  : F[Unit] =
    reporter.result(desc)(KlkResult.successful(result)) *>
    KlkResult.failures(result).traverse_(reporter.failure)

  def noop[F[_]: Applicative]: TestReporter[F] =
    NoopTestReporter()
}

case class NoopTestReporter[F[_]: Applicative]()
extends TestReporter[F]
{
  def result: String => Boolean => F[Unit] =
    _ => _ => ().pure[F]

  def failure: KlkResult.Details => F[Unit] =
    _ => ().pure[F]

  def fatal: Throwable => F[Unit] =
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

  implicit def Compute_IO: Compute[IO] =
    new Compute[IO] {
      def run[A](thunk: IO[A]): A =
        thunk
          .unsafeRunSync
    }

  implicit def Compute_Comonad[F[_]: Comonad]: Compute[F] =
    new Compute[F] {
      def run[A](thunk: F[A]): A =
        thunk.extract
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
  type Value

  def apply(output: Output): KlkResult[Value]
}

object TestResult
{
  type Aux[Output, V] = TestResult[Output] { type Value = V }

  implicit def TestResult_KlkResult[A]: TestResult.Aux[KlkResult[A], A] =
    new TestResult[KlkResult[A]] {
      type Value = A
      def apply(output: KlkResult[A]): KlkResult[A] =
        output
    }

  implicit def TestResult_Boolean: TestResult.Aux[Boolean, Unit] =
    new TestResult[Boolean] {
      type Value = Unit
      def apply(output: Boolean): KlkResult[Unit] =
        KlkResult(output)(KlkResult.Details.NoDetails)
    }

  implicit def TestResult_Either[A, B]
  (implicit inner: TestResult.Aux[B, Unit])
  : TestResult.Aux[Either[A, B], Unit] =
    new TestResult[Either[A, B]] {
      type Value = Unit
      def apply(output: Either[A, B]): KlkResult[Value] =
        output
          .map(inner(_))
          .valueOr(a => KlkResult.simpleFailure(NonEmptyList.one(a.toString)))
    }

  implicit def TestResult_Option[A]
  (implicit inner: TestResult.Aux[A, Unit])
  : TestResult.Aux[Option[A], Unit] =
    new TestResult[Option[A]] {
      type Value = Unit
      def apply(output: Option[A]): KlkResult[Value] =
        output
          .map(inner(_))
          .getOrElse(KlkResult.simpleFailure(NonEmptyList.one("test returned None")))
    }
}

trait Compile[F[_], G[_], A]
{
  type Value

  def apply(fa: F[A]): G[KlkResult[Value]]
}

object Compile
{
  type Aux[F[_], G[_], A, V] = Compile[F, G, A] { type Value = V }

  implicit def Compile_F_F[F[_]: Functor, A, V]
  (implicit result: TestResult.Aux[A, V])
  : Compile.Aux[F, F, A, V] =
    new Compile[F, F, A] {
      type Value = V

      def apply(fa: F[A]): F[KlkResult[Value]] =
        fa.map(result(_))
    }

  implicit def Compile_EitherT_F[F[_]: Functor, G[_], E, A, V]
  (implicit inner: Compile.Aux[F, G, Either[E, A], V])
  : Compile.Aux[EitherT[F, E, ?], G, A, V] =
    new Compile[EitherT[F, E, ?], G, A] {
      type Value = V
      def apply(fa: EitherT[F, E, A]): G[KlkResult[Value]] =
        inner(fa.value)
    }

  implicit def Compile_Stream[F[_], G[_], A, V]
  (implicit inner: Compile.Aux[F, G, Option[A], V], streamCompiler: Stream.Compiler[F, F])
  : Compile.Aux[Stream[F, *], G, A, V] =
    new Compile[Stream[F, *], G, A] {
      type Value = V

      def apply(fa: Stream[F, A]): G[KlkResult[Value]] =
        inner(fa.compile.last)
    }
}
