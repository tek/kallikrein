package klk

import scala.concurrent.ExecutionContext

import cats.data.OptionT
import cats.effect.{ContextShift, IO, Timer}
import org.http4s.{HttpRoutes, Request, Response}
import org.http4s.Status.Successful

class Http4sSuiteTest
extends KlkSpecification[IO]
{
  implicit def cs: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  implicit def timer: Timer[IO] =
    IO.timer(ExecutionContext.global)

  def routes: HttpRoutes[IO] =
    HttpRoutes.liftF(OptionT.pure(Response[IO]()))

  val test: Suite[IO, Unit, Unit] =
    Http4s.server[IO, NoopResources.type].routes(routes).test { res =>
      res.test("http4s") {
        case (client, uri) =>
          client.fetch(Request[IO](uri = uri)) {
            case Successful(_) => IO.pure(true)
            case _ => IO.pure(false)
          }
      }
    }

  "http4s test" >> {
    EvalSuite(test)
      .run(RunTestResources.cons(NoopResources))
      .map(_.map { case TestStats(desc, _, success, _, _) => (desc, success) })
      .map(_.must_==(List(("http4s", true))))
      .unsafeRunSync()
  }
}
