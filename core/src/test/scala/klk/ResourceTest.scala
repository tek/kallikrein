package klk

import cats.effect.{IO, Resource}

class ResourceTest
extends KlkSpecification[IO]
{
  // TODO 2.12 compat
  val res1: Resource[IO, Int] = Resource.pure[IO, Int](1)

  // TODO 2.12 compat
  val res2: Resource[IO, Int] = Resource.pure[IO, Int](1)

  val target: KlkResult[Unit] =
    KlkResult.success(KlkResult.Details.NoDetails)

  assert("resource")(_.resource(res1).resource(res2)((i: Int) => (j: Int) => IO.pure(i == j)))(target)
}
