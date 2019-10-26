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
  def apply[Params, Thunk]
  (thunk: TestShape[Thunk])
  (implicit transform: TransformTestThunk[RunF, TestRes, Params, Thunk], functor: Functor[TestShape])
  : TestShape[RunF[KlkResult]] =
    thunk.map(transform(resources))
}

case class AddTest[RunF[_], TestRes <: HList, Params, TestShape[_], AddResult]
(cons: ConsTest[RunF, TestRes, TestShape])
(add: TestShape[RunF[KlkResult]] => AddResult)
{
  def apply[Thunk]
  (thunk: TestShape[Thunk])
  (implicit transform: TransformTestThunk[RunF, TestRes, Params, Thunk], functor: Functor[TestShape])
  : AddResult =
    add(cons(thunk))
}

case class TestBuilder[RunF[_], TestRes <: HList, TestShape[_], AddResult]
(resources: TestResources[TestRes])
(add: TestShape[RunF[KlkResult]] => AddResult)
{
  def adder[Params, Output]: AddTest[RunF, TestRes, Params, TestShape, AddResult] =
    AddTest(ConsTest[RunF, TestRes, TestShape](resources))(add)

  def apply[Thunk]
  (thunk: TestShape[Thunk])
  (implicit transform: TransformTestThunk[RunF, TestRes, NoExecutionParams, Thunk], functor: Functor[TestShape])
  : AddResult =
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
  def cons[RunF[_], TestShape[_], AddResult]
  (add: TestShape[RunF[KlkResult]] => AddResult)
  : TestBuilder[RunF, HNil, TestShape, AddResult] =
    TestBuilder(TestResources.empty)(add)
}

case class TestThunkBuilder[RunF[_], TestRes <: HList, TestShape[_], AddResult]()
(resources: TestResources[TestRes])
(add: TestShape[RunF[KlkResult]] => AddResult)
{
  def adder[Params, Output]: AddTest[RunF, TestRes, Params, TestShape, AddResult] =
    AddTest(ConsTest[RunF, TestRes, TestShape](resources))(add)

  def apply[Thunk]
  (thunk: TestShape[Thunk])
  (implicit transform: TransformTestThunk[RunF, TestRes, NoExecutionParams, Thunk], functor: Functor[TestShape])
  : AddResult =
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
