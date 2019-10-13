package klk

import cats.effect.IO

trait SimpleTest[F[_]]
extends SimpleTestBase[F, SbtResources]

trait IOTest
extends SimpleTest[IO]
