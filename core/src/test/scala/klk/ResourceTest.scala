package klk

import cats.effect.{IO, Resource}

class ResourceTest
extends KlkSpecification[IO]
{
  val res1: Resource[IO, Int] = Resource.pure(1)

  val res2: Resource[IO, Int] = Resource.pure(1)

  val target: KlkResult =
    KlkResult.success(KlkResult.Details.NoDetails())

  assert("resource")(_.resource(res1).resource(res2)((i: Int) => (j: Int) => IO.pure(i == j)))(target)
}
