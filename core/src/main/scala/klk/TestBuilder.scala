package klk

import cats.Functor
import cats.effect.Resource
import cats.implicits._
import shapeless.{::, HList, HNil}

case class TestResources[ResParams <: HList](resources: ResParams)

object TestResources
{
  def empty: TestResources[HNil] =
    TestResources(HNil)
}

case class ConsTest[RunF[_], TestRes <: HList, TestShape[_], Output]
(resources: TestResources[TestRes])
(add: TestShape[RunF[KlkResult]] => Unit)
{
  def apply[Thunk]
  (thunk: TestShape[Thunk])
  (implicit transform: TransformTestThunk[RunF, TestRes, Thunk, Output], functor: Functor[TestShape])
  : Unit =
    add(thunk.map(transform(resources)))
}

case class TestBuilder[RunF[_], TestRes <: HList, TestShape[_]]
(resources: TestResources[TestRes])
(add: TestShape[RunF[KlkResult]] => Unit)
{
  def cons[Output]: ConsTest[RunF, TestRes, TestShape, Output] =
    ConsTest(resources)(add)

  def apply[TestF[_], Thunk, Output]
  (thunk: TestShape[Thunk])
  (implicit transform: TransformTestThunk[RunF, TestRes, Thunk, Output], functor: Functor[TestShape])
  : Unit =
    cons(thunk)

  def forallNoShrink: ConsTest[RunF, TestRes, TestShape, PropertyTestOutput[PropTrans.Full]] =
    cons

  def forall: ConsTest[RunF, TestRes, TestShape, PropertyTestOutput[PropTrans.Shrink]] =
    cons

  def resource[TestF[_], R]
  (res: Resource[TestF, R])
  : TestBuilder[RunF, Resource[TestF, R] :: TestRes, TestShape] =
    TestBuilder(TestResources(res :: resources.resources))(add)
}
