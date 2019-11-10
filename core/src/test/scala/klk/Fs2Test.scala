package klk

import cats.effect.IO
import fs2.Stream

class Fs2Test
extends KlkSpecification[IO]
{
  "compile a Stream" >>
  assert("stream")(_.apply(Stream[IO, Boolean](true)))(KlkResult.bool(true))
}
