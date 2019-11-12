package klk

import scala.concurrent.ExecutionContext

import cats.data.OptionT
import cats.effect.{ContextShift, IO, Timer}
import org.http4s.{HttpRoutes, Request, Response}

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
      res.test("http4s") { client =>
        client.fetch(Request[IO]())(_ => IO.pure(true))
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
