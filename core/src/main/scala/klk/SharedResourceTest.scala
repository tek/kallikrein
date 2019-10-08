package klk

import scala.collection.mutable

import cats.effect.{Resource, Sync}
import shapeless.{::, HList, HNil}

object SharedResourceTest
{
  def apply[TestF[_], RunF[_]: Sync, T, P, O, Res]
  (desc: String)
  (reporter: TestReporter)
  (thunk: Res => RunF[KlkResult])
  : KlkTest[RunF, Res] =
    KlkTest(desc, Test.execute(desc)(reporter)(thunk))
}

case class SharedResourceTestBuilder[RunF[_]: Sync, SharedRes, ResParams <: HList]
(desc: String)
(tests: mutable.Buffer[KlkTest[RunF, SharedRes]])
(reporter: TestReporter)
(resources: TestResources[ResParams])
{
  def apply[Thunk, Output]
  (thunk: SharedRes => Thunk)
  (implicit transform: TransformTestThunk[RunF, ResParams, Thunk, Output])
  : Unit =
    tests += SharedResourceTest(desc)(reporter)((transform(resources) _).compose(thunk))

  def resource[TestF[_], R]
  (r: Resource[TestF, R])
  : SharedResourceTestBuilder[RunF, SharedRes, Resource[TestF, R] :: ResParams] =
    SharedResourceTestBuilder(desc)(tests)(reporter)(TestResources(r :: resources.resources))
}

case class SharedResource[RunF[_]: Sync, SharedRes]
(resource: Resource[RunF, SharedRes])
(reporter: TestReporter)
(implicit val compute: Compute[RunF])
{
  val tests: mutable.Buffer[KlkTest[RunF, SharedRes]] =
    mutable.Buffer.empty

  def test(desc: String): SharedResourceTestBuilder[RunF, SharedRes, HNil] =
    SharedResourceTestBuilder(desc)(tests)(reporter)(TestResources.empty)
}
