package klk

import java.net.ServerSocket

import cats.MonadError
import cats.effect.{ConcurrentEffect, Resource, Sync, Timer}
import org.http4s.{HttpApp, HttpRoutes, Uri}
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

case class Port(number: Int)

object FreePort
{
  def find[F[_]: Sync]: F[Port] =
    Sync[F].bracket(Sync[F].delay(new ServerSocket(0))) { socket =>
      Sync[F].delay {
        socket.setReuseAddress(true)
        Port(socket.getLocalPort)
      }
    }(a => Sync[F].delay(a.close()))
}

object Http4s
{
  case class ServeRoutes[RunF[_]: Compute: MonadError[*[_], Throwable]: TestFramework[*[_], FR], FR]
  (app: HttpApp[RunF], builder: Option[BlazeServerBuilder[RunF]], client: Option[Resource[RunF, Client[RunF]]])
  (implicit ce: ConcurrentEffect[RunF], timer: Timer[RunF])
  {
    def withBuilder(newBuilder: BlazeServerBuilder[RunF]): ServeRoutes[RunF, FR] =
      copy(builder = Some(newBuilder))

    def test(tests: SharedResourceNonDsl[RunF, (Client[RunF], Uri), FR] => Suite[RunF, (Client[RunF], Uri), Unit])
    : Suite[RunF, Unit, Unit] = {
      val resource = Http4s.serverResource[RunF](app, builder, client)
      Suite.resource(resource, tests(SharedResourceNonDsl(resource)))
    }
  }

  case class Serve[RunF[_]: Compute: MonadError[*[_], Throwable]: TestFramework[*[_], FR], FR]()
  {
    def app
    (app: HttpApp[RunF])
    (implicit ce: ConcurrentEffect[RunF], timer: Timer[RunF])
    : ServeRoutes[RunF, FR] =
      ServeRoutes(app, None, None)

    def routes
    (r: HttpRoutes[RunF])
    (implicit ce: ConcurrentEffect[RunF], timer: Timer[RunF])
    : ServeRoutes[RunF, FR] =
      app(r.orNotFound)
  }

  def server[RunF[_]: Compute: MonadError[*[_], Throwable]: TestFramework[*[_], FR], FR]: Serve[RunF, FR] =
    Serve()

  def serverResource[RunF[_]]
  (app: HttpApp[RunF], builder: Option[BlazeServerBuilder[RunF]], client: Option[Resource[RunF, Client[RunF]]])
  (implicit ce: ConcurrentEffect[RunF], timer: Timer[RunF])
  : Resource[RunF, (Client[RunF], Uri)] =
    for {
      port <- Resource.liftF(FreePort.find[RunF])
      _ <- builder.getOrElse(BlazeServerBuilder[RunF])
        .bindHttp(port.number, "0.0.0.0")
        .withHttpApp(app)
        .resource
      client <- client.getOrElse(Concurrency.fixedPoolEc.flatMap(BlazeClientBuilder[RunF](_).resource))
    } yield (client, Uri(authority = Some(Uri.Authority(port = Some(port.number)))))
}

trait Http4s[RunF[_], FR]
{
  def server
  (implicit compute: Compute[RunF], me: MonadError[RunF, Throwable], fr: TestFramework[RunF, FR])
  : Http4s.Serve[RunF, FR] =
    Http4s.server[RunF, FR]
}
