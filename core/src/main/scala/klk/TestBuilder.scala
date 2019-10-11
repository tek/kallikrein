package klk

import cats.Functor
import cats.effect.Resource
import cats.implicits._
import org.typelevel.discipline.Laws
import shapeless.{::, HList, HNil}

case class TestResources[ResParams <: HList](resources: ResParams)

object TestResources
{
  def empty: TestResources[HNil] =
    TestResources(HNil)
}

case class ConsTest[RunF[_], TestRes <: HList, TestShape[_]]
(resources: TestResources[TestRes])
{
  def apply[Params, Thunk]
  (thunk: TestShape[Thunk])
  (implicit transform: TransformTestThunk[RunF, TestRes, Params, Thunk], functor: Functor[TestShape])
  : TestShape[RunF[KlkResult]] =
    thunk.map(transform(resources))
}

case class AddTest[RunF[_], TestRes <: HList, Params, TestShape[_]]
(cons: ConsTest[RunF, TestRes, TestShape])
(add: TestShape[RunF[KlkResult]] => Unit)
{
  def apply[Thunk]
  (thunk: TestShape[Thunk])
  (implicit transform: TransformTestThunk[RunF, TestRes, Params, Thunk], functor: Functor[TestShape])
  : Unit =
    add(cons(thunk))
}

case class TestBuilder[RunF[_], TestRes <: HList, TestShape[_]]
(resources: TestResources[TestRes])
(add: TestShape[RunF[KlkResult]] => Unit)
{
  def adder[Params, Output]: AddTest[RunF, TestRes, Params, TestShape] =
    AddTest(ConsTest[RunF, TestRes, TestShape](resources))(add)

  def apply[TestF[_], Thunk]
  (thunk: TestShape[Thunk])
  (implicit transform: TransformTestThunk[RunF, TestRes, NoExecutionParams, Thunk], functor: Functor[TestShape])
  : Unit =
    adder(thunk)

  def forallNoShrink: AddTest[RunF, TestRes, PropTrans.Full, TestShape] =
    adder

  def forall: AddTest[RunF, TestRes, PropTrans.Shrink, TestShape] =
    adder

  def laws: AddTest[RunF, TestRes, LawsParams, TestShape] =
    adder

  def resource[R]
  (res: Resource[RunF, R])
  : TestBuilder[RunF, Resource[RunF, R] :: TestRes, TestShape] =
    TestBuilder(TestResources(res :: resources.resources))(add)
}
