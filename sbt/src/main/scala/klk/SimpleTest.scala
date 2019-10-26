package klk

import cats.effect.IO

trait SimpleTest[F[_]]
extends SimpleTestBase[F, SbtResources]

trait IOTest
extends SimpleTest[IO]

trait SimpleComposeTest[F[_]]
extends ComposeTest[F, SbtResources]

trait IOComposeTest
extends SimpleComposeTest[IO]
