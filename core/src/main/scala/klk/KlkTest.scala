package klk

import scala.util.control.NonFatal

import cats.{Functor, MonadError}
import cats.effect.{Bracket, Resource}

case class KlkTest[F[_], Res](desc: String, thunk: TestReporter[F] => Res => F[KlkResult])

object KlkTest
{
  def runPlain[F[_]: Functor, FR]
  (test: KlkTest[F, Unit])
  (fwRes: FR)
  (implicit compute: Compute[F], fw: TestFramework[F, FR])
  : KlkResult =
    compute.run(test.thunk(fw.reporter(fwRes))(()))

  def runResource[F[_]: Bracket[*[_], Throwable], Res, FR]
  (resource: Resource[F, Res])
  (tests: collection.Seq[KlkTest[F, Res]])
  (fwRes: FR)
  (implicit compute: Compute[F], fw: TestFramework[F, FR])
  : KlkResult =
    compute.run(resource.use(r => tests.toList.traverse(_.thunk(fw.reporter(fwRes))(r))).map(_.combineAll))

  def executeThunk[RunF[_]: MonadError[*[_], Throwable], SharedRes, FR]
  (desc: String)
  (thunk: SharedRes => RunF[KlkResult])
  (reporter: TestReporter[RunF])
  (sharedRes: SharedRes)
  : RunF[KlkResult] =
    for {
      testResult <- thunk(sharedRes)
        .recover { case NonFatal(a) => KlkResult.failure(KlkResult.Details.Fatal(a)) }
        _ <- TestReporter.report[RunF](reporter)(desc)(testResult)
    } yield testResult

  def cons[RunF[_]: MonadError[*[_], Throwable], Res](desc: String)(thunk: Res => RunF[KlkResult]): KlkTest[RunF, Res] =
    KlkTest(desc, KlkTest.executeThunk(desc)(thunk))

  def plain[RunF[_]: MonadError[*[_], Throwable]](desc: String)(thunk: RunF[KlkResult]): KlkTest[RunF, Unit] =
    cons(desc)(_ => thunk)
}
