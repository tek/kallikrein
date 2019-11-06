package klk

import cats.effect.IO

class ExceptionTest
extends KlkSpecification[IO]
{
  object E
  extends Throwable

  def frame1: Boolean =
    throw E

  val target: KlkResult[Unit] =
    KlkResult.Fatal(E)

  assert("exception")(_.apply(IO(frame1)))(target)
}
