package klk

import java.net.ServerSocket

import cats.Functor
import cats.effect.{Resource, Sync}
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
  case class ServeRoutes[RunF[_], FR]
  (app: HttpApp[RunF], builder: Option[BlazeServerBuilder[RunF]], client: Option[Resource[RunF, Client[RunF]]])
  {
    def withBuilder(newBuilder: BlazeServerBuilder[RunF]): ServeRoutes[RunF, FR] =
      copy(builder = Some(newBuilder))

    def test[A]
    (tests: SharedResource[RunF, (Client[RunF], Uri), FR] => Suite[RunF, (Client[RunF], Uri), A])
    (
      implicit
      sync: Sync[RunF],
      compute: Compute[RunF],
      framework: TestFramework[RunF, FR],
      consConcurrent: ConsConcurrent[RunF],
      consTimer: ConsTimer[RunF],
    )
    : Suite[RunF, Unit, A] =
      SharedResource.suite(Http4s.serverResource[RunF](app, builder, client))(tests)
  }

  case class Serve[RunF[_], FR]()
  {
    def app
    (app: HttpApp[RunF])
    : ServeRoutes[RunF, FR] =
      ServeRoutes(app, None, None)

    def routes
    (r: HttpRoutes[RunF])
    (implicit functor: Functor[RunF])
    : ServeRoutes[RunF, FR] =
      app(r.orNotFound)
  }

  def server[RunF[_], FR]: Serve[RunF, FR] =
    Serve()

  def serverResource[RunF[_]: Sync]
  (app: HttpApp[RunF], builder: Option[BlazeServerBuilder[RunF]], client: Option[Resource[RunF, Client[RunF]]])
  (implicit consConcurrent: ConsConcurrent[RunF], consTimer: ConsTimer[RunF])
  : Resource[RunF, (Client[RunF], Uri)] =
    Concurrency.fixedPoolEc[RunF].flatMap { ec =>
      for {
        port <- Resource.liftF(FreePort.find[RunF])
        _ <- builder.getOrElse(BlazeServerBuilder[RunF](consConcurrent(ec), consTimer(ec)))
          .bindHttp(port.number, "0.0.0.0")
          .withHttpApp(app)
          .resource
        client <- client.getOrElse(
          Concurrency.fixedPoolEc.flatMap(ec => BlazeClientBuilder[RunF](ec)(consConcurrent(ec)).resource)
        )
      } yield (client, Uri(authority = Some(Uri.Authority(port = Some(port.number)))))
    }
}

trait Http4s[RunF[_], FR]
{
  def server
  : Http4s.Serve[RunF, FR] =
    Http4s.server[RunF, FR]
}
