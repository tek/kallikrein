package klk

import cats.effect.Bracket
import shapeless.HList

trait TransformTestThunk[RunF[_], ResParams <: HList, Thunk, A]
{
  def apply(resources: TestResources[ResParams])(thunk: Thunk): RunF[KlkResult]
}

object TransformTestThunk
{
  implicit def TransformTestThunk_Any
  [RunF[_]: Bracket[*[_], Throwable], ResParams <: HList, Thunk, Thunk0, TestF[_], Output]
  (
    implicit
    strip: StripResources.Aux[RunF, ResParams, Thunk, Thunk0],
    execute: ExecuteThunk.Aux[Thunk0, Output, TestF],
    compile: Compile[TestF, RunF],
    result: TestResult[RunF, Output],
  )
  : TransformTestThunk[RunF, ResParams, Thunk, Output] =
    new TransformTestThunk[RunF, ResParams, Thunk, Output] {
      def apply(resources: TestResources[ResParams])(thunk: Thunk): RunF[KlkResult] =
        strip(resources)(thunk).use(t => result.handle(compile(execute(t))))
    }
}
