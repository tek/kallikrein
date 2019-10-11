package klk

import cats.effect.Bracket
import shapeless.HList

trait TransformTestThunk[RunF[_], ResParams <: HList, Params, Thunk]
{
  def apply(resources: TestResources[ResParams])(thunk: Thunk): RunF[KlkResult]
}

object TransformTestThunk
{
  implicit def TransformTestThunk_Any
  [RunF[_]: Bracket[*[_], Throwable], ResParams <: HList, Params, Thunk, Thunk0, TestF[_], Output]
  (
    implicit
    strip: StripResources.Aux[RunF, ResParams, Thunk, Thunk0],
    execute: ExecuteThunk.Aux[Params, Thunk0, TestF, Output],
    compile: Compile[TestF, RunF, Output],
  )
  : TransformTestThunk[RunF, ResParams, Params, Thunk] =
    new TransformTestThunk[RunF, ResParams, Params, Thunk] {
      def apply(resources: TestResources[ResParams])(thunk: Thunk): RunF[KlkResult] =
        strip(resources)(thunk).use(t => compile(execute(t)))
    }
}
