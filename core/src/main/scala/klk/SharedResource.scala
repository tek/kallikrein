package klk

import scala.collection.mutable

import cats.MonadError
import cats.data.Const
import cats.effect.{Bracket, Resource}
import shapeless.HNil

case class SharedResource[RunF[_]: Compute: MonadError[*[_], Throwable]: TestFramework[*[_], FR], SharedRes, FR]
(resource: Resource[RunF, SharedRes])
{
  private[this] case class Cons(desc: String)
  extends TestAdder[RunF, SharedRes => *, Suite[RunF, SharedRes, *]]
  {
    def apply[A](thunk: SharedRes => RunF[KlkResult[A]]): Suite[RunF, SharedRes, A] =
      Suite.single(KlkTest.cons(desc)(thunk))
  }

  def test(desc: String): TestBuilder[RunF, HNil, SharedRes => *, Suite[RunF, SharedRes, *]] =
    TestBuilder(TestResources.empty)(Cons(desc))
}

object SharedResource
{
  def suite[RunF[_]: Compute: TestFramework[*[_], FR]: Bracket[*[_], Throwable], SharedRes, FR, A]
  (resource: Resource[RunF, SharedRes])
  (tests: SharedResource[RunF, SharedRes, FR] => Suite[RunF, SharedRes, A])
  : Suite[RunF, Unit, A] =
    Suite.resource(resource, tests(SharedResource(resource)))
}

case class DslSharedResource[RunF[_]: MonadError[*[_], Throwable], SharedRes]
(tests: mutable.Buffer[Suite[RunF, SharedRes, Unit]])
{
  private[this] case class Add(desc: String)
  extends TestAdder[RunF, SharedRes => *, Const[Unit, *]]
  {
    def apply[A](thunk: SharedRes => RunF[KlkResult[A]]): Const[Unit, A] = {
      tests += Suite.single(KlkTest.cons(desc)(thunk)).void
      Const(())
    }
  }

  def test(desc: String): TestBuilder[RunF, HNil, Function1[SharedRes, *], Const[Unit, *]] =
    TestBuilder(TestResources.empty)(Add(desc))
}

object DslSharedResource
{
  def cons[RunF[_]: MonadError[*[_], Throwable], SharedRes]: DslSharedResource[RunF, SharedRes] =
    DslSharedResource(mutable.Buffer.empty)
}
