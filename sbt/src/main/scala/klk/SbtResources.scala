package klk

import cats.{Functor, Traverse}
import cats.data.NonEmptyList
import cats.effect.Sync
import sbt.testing.Logger

case class SbtTestLog(loggers: Array[Logger])

object SbtTestLog
{
  def sync[F[_]: Sync, T[_]: Traverse](log: SbtTestLog)(f: Logger => String => Unit): T[String] => F[Unit] =
    lines =>
      log.loggers.toList.traverse_(logger => lines.traverse_(line => Sync[F].delay(f(logger)(line))))

  def unsafe[T[_]: Functor](log: SbtTestLog)(f: Logger => String => Unit)(lines: T[String]): Unit =
      log.loggers.toList.foreach(logger => lines.map(line => f(logger)(line)))
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
      SbtTestLog.sync[F, NonEmptyList](log)(if (success) _.info else _.error)
        .apply(TestReporter.formatResult(desc)(success))

  def failure: KlkResult.Details => F[Unit] =
    SbtTestLog.sync[F, NonEmptyList](log)(_.error).compose(Indent[NonEmptyList](2)).compose(TestReporter.formatFailure)
}
