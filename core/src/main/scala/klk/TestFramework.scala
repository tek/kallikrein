package klk

import cats.effect.Sync

trait TestFramework[RunF[_], Resources]
{
  def reporter(res: Resources): TestReporter[RunF]
}

object TestFramework
{
  implicit def TestFramework_Sbt[RunF[_]: Sync]: TestFramework[RunF, SbtResources] =
    new TestFramework[RunF, SbtResources] {
      def reporter(res: SbtResources): TestReporter[RunF] =
        SbtTestReporter(res.log)
    }
}

case class SbtResources(log: SbtTestLog)
