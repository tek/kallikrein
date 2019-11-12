package klk

import cats.effect.IO
import org.http4s.{HttpApp, Request, Response}

class Http4sSbtTest
extends Http4sIOTest
{
  def tests: Suite[IO, Unit, Unit] =
    server
      .app(HttpApp.liftF(IO.pure(Response[IO]())))
      .test { builder =>
      builder.test("http4s") { client =>
        client.fetch(Request[IO]())(_ => IO.pure(true))
      }
    }
}
