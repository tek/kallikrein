package klk

import cats.{Id, MonadError}
import cats.effect.{Bracket, Resource}
import shapeless.HNil

abstract class ComposeTest[RunF[_]: MonadError[*[_], Throwable]: Compute: MeasureTest: TestFramework[*[_], FR], FR]
extends TestBase[RunF, FR]
{
  private[this] def cons(desc: String)(thunk: Id[RunF[KlkResult]]): TestAlg[RunF, Unit, Unit] =
    TestAlg.single(KlkTest.plain(desc)(thunk))

  def test(desc: String): TestBuilder[RunF, HNil, Id, TestAlg[RunF, Unit, Unit]] =
    TestBuilder(TestResources.empty)(cons(desc))

  def sharedResource[R]
  (resource: Resource[RunF, R])
  (tests: SharedResourceNonDsl[RunF, R, FR] => TestAlg[RunF, R, Unit])
  (implicit bracket: Bracket[RunF, Throwable])
  : TestAlg[RunF, Unit, Unit] =
    TestAlg.resource(resource, tests(SharedResourceNonDsl(resource)))
}
