package klk

import cats.effect.Sync

trait TestFramework[RunF[_], Resources]
{
  def reporter(res: Resources): TestReporter[RunF]
}

object TestFramework
{
  implicit def TestFramework_SbtResources[RunF[_]: Sync]: TestFramework[RunF, SbtResources] =
    new TestFramework[RunF, SbtResources] {
      def reporter(res: SbtResources): TestReporter[RunF] =
        SbtTestReporter(res.log)
    }

  implicit def TestFramework_NoopResources[RunF[_]: Sync]: TestFramework[RunF, NoopResources.type] =
    new TestFramework[RunF, NoopResources.type] {
      def reporter(res: NoopResources.type): TestReporter[RunF] =
        NoopTestReporter()
    }
}

case class SbtResources(log: SbtTestLog)

object NoopResources
