package klk

import cats.Monad

private[klk] trait TestMarker

trait FrameworkTest[FR]
extends TestMarker
{
  def run(frameworkResources: FR): List[TestStats]

}

abstract class TestBase[RunF[_]: Monad: Compute: MeasureTest: TestFramework[*[_], FR], FR]
extends FrameworkTest[FR]
{
  def tests: Suite[RunF, Unit, Unit]

  def run(frameworkResources: FR): List[TestStats] =
    Compute[RunF].run(EvalSuite(tests).run(RunTestResources.cons(frameworkResources)))
}
