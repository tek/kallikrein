package klk

import scala.collection.mutable

import cats.effect.{Resource, Sync}
import shapeless.{::, HList, HNil}

case class SharedResourceTestBuilder[RunF[_], SharedRes, TestRes <: HList]
(resources: TestResources[TestRes])
(add: (SharedRes => RunF[KlkResult]) => Unit)
{
  def apply[Thunk, Output]
  (thunk: SharedRes => Thunk)
  (implicit transform: TransformTestThunk[RunF, TestRes, Thunk, Output])
  : Unit =
    add((transform(resources) _).compose(thunk))

  def resource[TestF[_], R]
  (res: Resource[TestF, R])
  : SharedResourceTestBuilder[RunF, SharedRes, Resource[TestF, R] :: TestRes] =
    SharedResourceTestBuilder(TestResources(res :: resources.resources))(add)
}

case class SharedResource[RunF[_]: Sync, SharedRes]
(compute: Compute[RunF], resource: Resource[RunF, SharedRes], tests: mutable.Buffer[KlkTest[RunF, SharedRes]])
(reporter: TestReporter)
{
  def add(desc: String)(thunk: SharedRes => RunF[KlkResult]): Unit =
    tests += KlkTest(desc, Test.execute(desc)(reporter)(thunk))

  def test(desc: String): SharedResourceTestBuilder[RunF, SharedRes, HNil] =
    SharedResourceTestBuilder(TestResources.empty)(add(desc))
}
