package klk

import scala.concurrent.ExecutionContext

import cats.{Applicative, Functor}
import cats.data.EitherT
import cats.effect.{Concurrent, IO, Sync}
import cats.implicits._
import sbt.testing.Logger

case class SbtTestLog(loggers: Array[Logger])

object SbtTestLog
{
  def sync[F[_]: Sync](log: SbtTestLog)(f: Logger => String => Unit): List[String] => F[Unit] =
    lines =>
      log.loggers.toList.traverse_(logger => lines.traverse_(line => Sync[F].delay(f(logger)(line))))

  def unsafe(log: SbtTestLog)(f: Logger => String => Unit)(lines: List[String]): Unit =
      log.loggers.toList.foreach(logger => lines.foreach(line => f(logger)(line)))
}

case class SbtResources(log: SbtTestLog)

object SbtResources
extends SbtResourcesInstances

trait SbtResourcesInstances
{
  implicit def TestFramework_SbtResources[RunF[_]: Sync]: TestFramework[RunF, SbtResources] =
    new TestFramework[RunF, SbtResources] {
      def reporter(res: SbtResources): TestReporter[RunF] =
        SbtTestReporter(res.log)
    }
}

case class SbtTestReporter[F[_]: Sync](log: SbtTestLog)
extends TestReporter[F]
{
  def result: String => Boolean => F[Unit] =
    desc => success =>
      SbtTestLog.sync[F](log)(if (success) _.info else _.error).apply(TestReporter.formatResult(desc)(success))

  def failure: KlkResult.Details => F[Unit] =
    SbtTestLog.sync[F](log)(_.error).compose(Indent(2)).compose(TestReporter.formatFailure)
}
