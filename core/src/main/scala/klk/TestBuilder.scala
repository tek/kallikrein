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
{
  def apply[Thunk]
  (thunk: TestShape[Thunk])
  (implicit transform: TransformTestThunk[RunF, TestRes, Thunk, Output], functor: Functor[TestShape])
  : TestShape[RunF[KlkResult]] =
    thunk.map(transform(resources))
}

case class AddTest[RunF[_], TestRes <: HList, TestShape[_], Output]
(cons: ConsTest[RunF, TestRes, TestShape, Output])
(add: TestShape[RunF[KlkResult]] => Unit)
{
  def apply[Thunk]
  (thunk: TestShape[Thunk])
  (implicit transform: TransformTestThunk[RunF, TestRes, Thunk, Output], functor: Functor[TestShape])
  : Unit =
    add(cons(thunk))
}

case class TestBuilder[RunF[_], TestRes <: HList, TestShape[_]]
(resources: TestResources[TestRes])
(add: TestShape[RunF[KlkResult]] => Unit)
{
  def adder[Output]: AddTest[RunF, TestRes, TestShape, Output] =
    AddTest(ConsTest[RunF, TestRes, TestShape, Output](resources))(add)

  def apply[TestF[_], Thunk, Output]
  (thunk: TestShape[Thunk])
  (implicit transform: TransformTestThunk[RunF, TestRes, Thunk, Output], functor: Functor[TestShape])
  : Unit =
    adder(thunk)

  def forallNoShrink: AddTest[RunF, TestRes, TestShape, PropertyTestOutput[PropTrans.Full]] =
    adder

  def forall: AddTest[RunF, TestRes, TestShape, PropertyTestOutput[PropTrans.Shrink]] =
    adder

  def resource[TestF[_], R]
  (res: Resource[TestF, R])
  : TestBuilder[RunF, Resource[TestF, R] :: TestRes, TestShape] =
    TestBuilder(TestResources(res :: resources.resources))(add)
}
