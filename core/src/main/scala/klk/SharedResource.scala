package klk

import scala.collection.mutable

import cats.MonadError
import cats.effect.Resource
import shapeless.HNil

case class SharedResourceNonDsl[RunF[_]: Compute: MonadError[*[_], Throwable]: TestFramework[*[_], FR], SharedRes, FR]
(resource: Resource[RunF, SharedRes])
{
  private[this] def cons(desc: String)(thunk: SharedRes => RunF[KlkResult]): TestAlg[RunF, SharedRes, Unit] =
    TestAlg.single(KlkTest.cons(desc)(thunk))

  def test(desc: String): TestBuilder[RunF, HNil, Function1[SharedRes, *], TestAlg[RunF, SharedRes, Unit]] =
    TestBuilder(TestResources.empty)(cons(desc))
}

case class SharedResource[RunF[_]: MonadError[*[_], Throwable], SharedRes]
(tests: mutable.Buffer[TestAlg[RunF, SharedRes, Unit]])
{
  def add(desc: String)(thunk: SharedRes => RunF[KlkResult]): Unit =
    tests += TestAlg.single(KlkTest.cons(desc)(thunk))

  def test(desc: String): TestBuilder[RunF, HNil, Function1[SharedRes, *], Unit] =
    TestBuilder(TestResources.empty)(add(desc))
}

object SharedResource
{
  def cons[RunF[_]: MonadError[*[_], Throwable], SharedRes]: SharedResource[RunF, SharedRes] =
    SharedResource(mutable.Buffer.empty)
}
