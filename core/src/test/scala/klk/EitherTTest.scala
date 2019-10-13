package klk

import cats.data.EitherT
import cats.effect.IO

class EitherTTest
extends KlkSpecification[IO]
{
  val target: KlkResult =
    KlkResult.Single(true, KlkResult.Details.NoDetails())

  assert("EitherT")(_(EitherT.right[Unit](IO.pure(1 == 1))))(target)
}
