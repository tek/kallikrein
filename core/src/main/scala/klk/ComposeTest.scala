package klk

import cats.{Id, MonadError}
import cats.effect.{Bracket, Resource}
import shapeless.HNil

abstract class ComposeTest[RunF[_]: MonadError[*[_], Throwable]: Compute: MeasureTest: TestFramework[*[_], FR], FR]
extends TestBase[RunF, FR]
{
  private[this] case class Cons(desc: String)
  extends TestAdder[RunF, Id, Suite[RunF, Unit, *]]
  {
    def apply[A](thunk: Id[RunF[KlkResult[A]]]): Suite[RunF, Unit, A] =
      Suite.single(KlkTest.plain(desc)(thunk))
  }

  def test(desc: String): TestBuilder[RunF, HNil, Id, Suite[RunF, Unit, *]] =
    TestBuilder(TestResources.empty)(Cons(desc))

  def sharedResource[R]
  (resource: Resource[RunF, R])
  (tests: SharedResourceNonDsl[RunF, R, FR] => Suite[RunF, R, Unit])
  (implicit bracket: Bracket[RunF, Throwable])
  : Suite[RunF, Unit, Unit] =
    Suite.resource(resource, tests(SharedResourceNonDsl(resource)))
}
