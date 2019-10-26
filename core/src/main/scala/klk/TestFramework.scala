package klk

import cats.Applicative

trait TestFramework[RunF[_], Resources]
{
  def reporter(res: Resources): TestReporter[RunF]
}

object TestFramework
{
  implicit def TestFramework_NoopResources[RunF[_]: Applicative]: TestFramework[RunF, NoopResources.type] =
    new TestFramework[RunF, NoopResources.type] {
      def reporter(res: NoopResources.type): TestReporter[RunF] =
        NoopTestReporter()
    }
}

object NoopResources
