package klk

import cats.effect.IO

trait Http4sTest[F[_]]
extends Http4sTestBase[F, SbtResources]

trait Http4sIOTest
extends Http4sTest[IO]
