package klk

import scala.concurrent.ExecutionContext

import cats.effect.{ContextShift, IO, Timer}
import org.http4s.{HttpApp, Request, Response}
import org.http4s.Status.Successful

class Http4sSbtTest
extends Http4sIOTest
{
  implicit def cs: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  implicit def timer: Timer[IO] =
    IO.timer(ExecutionContext.global)

  def tests: Suite[IO, Unit, Unit] =
    server
      .app(HttpApp.liftF(IO.pure(Response[IO]())))
      .test { builder =>
      builder.test("http4s") {
        case (client, uri) =>
          client.fetch(Request[IO](uri = uri)) {
            case Successful(_) => IO.pure(true)
            case _ => IO.pure(false)
          }
      }
    }
}
