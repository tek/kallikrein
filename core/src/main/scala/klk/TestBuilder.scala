package klk

import cats.Functor
import cats.effect.Resource
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
  def apply[Params, Thunk, Value]
  (thunk: TestShape[Thunk])
  (implicit transform: TransformTestThunk.Aux[RunF, TestRes, Params, Thunk, Value], functor: Functor[TestShape])
  : TestShape[RunF[KlkResult[Value]]] =
    thunk.map(transform(resources)(_))
}

trait TestAdder[RunF[_], TestShape[_], AddResult[_]]
{
  def apply[A](test: TestShape[RunF[KlkResult[A]]]): AddResult[A]
}

case class AddTest[RunF[_], TestRes <: HList, Params, TestShape[_], AddResult[_]]
(cons: ConsTest[RunF, TestRes, TestShape])
(add: TestAdder[RunF, TestShape, AddResult])
{
  def apply[Thunk, Value]
  (thunk: TestShape[Thunk])
  (implicit transform: TransformTestThunk.Aux[RunF, TestRes, Params, Thunk, Value], functor: Functor[TestShape])
  : AddResult[Value] =
    add(cons(thunk))
}

case class TestBuilder[RunF[_], TestRes <: HList, TestShape[_], AddResult[_]]
(resources: TestResources[TestRes])
(add: TestAdder[RunF, TestShape, AddResult])
{
  def adder[Params, Output]: AddTest[RunF, TestRes, Params, TestShape, AddResult] =
    AddTest(ConsTest[RunF, TestRes, TestShape](resources))(add)

  def apply[Thunk, Value]
  (thunk: TestShape[Thunk])
  (implicit transform: TransformTestThunk.Aux[RunF, TestRes, NoExecutionParams, Thunk, Value], functor: Functor[TestShape])
  : AddResult[Value] =
    adder(thunk)

  def forallNoShrink: AddTest[RunF, TestRes, PropTrans.Full, TestShape, AddResult] =
    adder

  def forall: AddTest[RunF, TestRes, PropTrans.Shrink, TestShape, AddResult] =
    adder

  def laws: AddTest[RunF, TestRes, LawsParams, TestShape, AddResult] =
    adder

  def resource[R]
  (res: Resource[RunF, R])
  : TestBuilder[RunF, Resource[RunF, R] :: TestRes, TestShape, AddResult] =
    TestBuilder(TestResources(res :: resources.resources))(add)
}

object TestBuilder
{
  def cons[RunF[_], TestShape[_], AddResult[_]]
  (add: TestAdder[RunF, TestShape, AddResult])
  : TestBuilder[RunF, HNil, TestShape, AddResult] =
    TestBuilder(TestResources.empty)(add)
}
