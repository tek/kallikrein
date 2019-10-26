package klk

import cats.data.NonEmptyList
import cats.effect.{IO, Resource}

import shapeless.HNil

class SharedResTest
extends KlkSharedResourceSpecification[IO, Int]
{
  def resource: Resource[IO, Int] =
    Resource.pure(86)

  val testResource: Resource[IO, Int] = Resource.pure(4)

  def srTest(builder: TestBuilder[IO, HNil, Function1[Int, ?], Int => IO[KlkResult]]): List[Int => IO[KlkResult]] =
    List(
      builder(i => IO.pure(i == 86)),
      builder(i => IO.pure(i == 68)),
      builder.resource(testResource)((i: Int) => (j: Int) => IO.pure(i + j < 90))
    )

  val target: KlkResult =
    KlkResult.Multi(
      NonEmptyList.of(
        KlkResult.success(KlkResult.Details.NoDetails()),
        KlkResult.failure(KlkResult.Details.NoDetails()),
        KlkResult.failure(KlkResult.Details.NoDetails()),
      )
    )

  assert("shared resource")(srTest)(target)
}
