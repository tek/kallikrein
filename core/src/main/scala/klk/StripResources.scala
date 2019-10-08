package klk

import cats.Applicative
import cats.effect.{Bracket, Resource}
import shapeless.{::, HList, HNil}

trait StripResources[F[_], ResParams <: HList, ThunkF]
{
  type Thunk

  def apply(resources: TestResources[ResParams])(thunk: ThunkF): Resource[F, Thunk]
}

object StripResources
{
  type Aux[F[_], ResParams <: HList, ThunkF, Thunk0] =
    StripResources[F, ResParams, ThunkF] {
      type Thunk = Thunk0
    }

  implicit def StripResources_HNil[TestF[_], RunF[_]: Applicative, Output]
  : StripResources.Aux[RunF, HNil, TestF[Output], TestF[Output]] =
    new StripResources[RunF, HNil, TestF[Output]] {
      type Thunk = TestF[Output]
      def apply(resources: TestResources[HNil])(thunk: Thunk): Resource[RunF, Thunk] =
        Resource.pure(thunk)
    }

  implicit def StripResources_HList[TestF[_], RunF[_]: Bracket[*[_], Throwable], H, T <: HList, ThunkF, Output]
  (implicit next: StripResources.Aux[RunF, T, ThunkF, TestF[Output]])
  : Aux[RunF, Resource[RunF, H] :: T, H => ThunkF, TestF[Output]] =
    new StripResources[RunF, Resource[RunF, H] :: T, H => ThunkF] {
      type Thunk = TestF[Output]
      def apply(resources: TestResources[Resource[RunF, H] :: T])(thunk: H => ThunkF): Resource[RunF, TestF[Output]] =
        for {
          h <- resources.resources.head
          t <- next(TestResources(resources.resources.tail))(thunk(h))
        } yield t
    }
}
