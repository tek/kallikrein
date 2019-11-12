package klk

import java.net.ServerSocket

import cats.{Functor, MonadError}
import cats.data.{EitherT, NonEmptyList}
import cats.effect.{Resource, Sync}
import org.http4s.{EntityDecoder, HttpApp, HttpRoutes, Request, Response, Status, Uri}
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

case class TestClient[F[_]](client: Client[F], port: Port)
{
  def authority: Uri.Authority =
    Uri.Authority(port = Some(port.number))

  def uri(uri: Uri): Uri =
    uri.copy(authority = Some(authority))

  def withUri(request: Request[F]): Request[F] =
    request.withUri(uri(request.uri))

  def fetch[A](request: Request[F])(f: Response[F] => F[A]): F[A] =
    client.fetch(withUri(request))(f)

  def success[A]
  (request: Request[F])
  (f: Response[F] => F[A])
  (implicit monadError: MonadError[F, Throwable], decoder: EntityDecoder[F, String])
  : EitherT[F, KlkResult[Unit], A] =
    EitherT {
      fetch(request) {
        case Status.Successful(response) =>
          f(response).map(Right(_))
        case response =>
          response.as[String]
            .map(body => Left(KlkResult.simpleFailure(NonEmptyList.one(s"request failed: $body ($response)"))))
      }
    }
}

object Http4s
{
  case class ServeRoutes[RunF[_], FR]
  (app: HttpApp[RunF], builder: Option[BlazeServerBuilder[RunF]], client: Option[Resource[RunF, Client[RunF]]])
  {
    def withBuilder(newBuilder: BlazeServerBuilder[RunF]): ServeRoutes[RunF, FR] =
      copy(builder = Some(newBuilder))

    def test[A]
    (tests: SharedResource[RunF, TestClient[RunF], FR] => Suite[RunF, TestClient[RunF], A])
    (
      implicit
      sync: Sync[RunF],
      compute: Compute[RunF],
      framework: TestFramework[RunF, FR],
      consConcurrent: ConsConcurrent[RunF],
      consTimer: ConsTimer[RunF],
    )
    : Suite[RunF, Unit, A] =
      SharedResource.suite(Http4s.serverResource[RunF](consConcurrent, consTimer)(app, builder, client))(tests)
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

  def defaultClient[RunF[_]: Sync](consConcurrent: ConsConcurrent[RunF]): Resource[RunF, Client[RunF]] =
    Concurrency.fixedPoolEc.flatMap(ec => BlazeClientBuilder[RunF](ec)(consConcurrent(ec)).resource)

  def serverResource[RunF[_]: Sync]
  (consConcurrent: ConsConcurrent[RunF], consTimer: ConsTimer[RunF])
  (app: HttpApp[RunF], builder: Option[BlazeServerBuilder[RunF]], client: Option[Resource[RunF, Client[RunF]]])
  : Resource[RunF, TestClient[RunF]] =
    Concurrency.fixedPoolEc[RunF].flatMap { ec =>
      for {
        port <- Resource.liftF(FreePort.find[RunF])
        _ <- builder.getOrElse(BlazeServerBuilder[RunF](consConcurrent(ec), consTimer(ec)))
          .bindHttp(port.number, "0.0.0.0")
          .withHttpApp(app)
          .resource
        client <- client.getOrElse(defaultClient(consConcurrent))
      } yield TestClient(client, port)
    }
}

trait Http4s[RunF[_], FR]
{
  def server
  : Http4s.Serve[RunF, FR] =
    Http4s.server[RunF, FR]
}
