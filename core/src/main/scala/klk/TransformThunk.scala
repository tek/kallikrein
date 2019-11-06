package klk

import cats.effect.Bracket
import shapeless.HList

trait TransformTestThunk[RunF[_], ResParams <: HList, Params, Thunk]
{
  type Value

  def apply(resources: TestResources[ResParams])(thunk: Thunk): RunF[KlkResult[Value]]
}

object TransformTestThunk
{
  type Aux[RunF[_], ResParams <: HList, Params, Thunk, V] =
    TransformTestThunk[RunF, ResParams, Params, Thunk] { type Value = V }

  implicit def TransformTestThunk_Any
  [RunF[_]: Bracket[*[_], Throwable], ResParams <: HList, Params, Thunk, Thunk0, TestF[_], Output, V]
  (
    implicit
    strip: StripResources.Aux[RunF, ResParams, Thunk, Thunk0],
    execute: ExecuteThunk.Aux[Params, Thunk0, TestF, Output],
    compile: Compile.Aux[TestF, RunF, Output, V],
  )
  : TransformTestThunk.Aux[RunF, ResParams, Params, Thunk, V] =
    new TransformTestThunk[RunF, ResParams, Params, Thunk] {
      type Value = V
      def apply(resources: TestResources[ResParams])(thunk: Thunk): RunF[KlkResult[Value]] =
        strip(resources)(thunk).use(t => compile(execute(t)))
    }
}
