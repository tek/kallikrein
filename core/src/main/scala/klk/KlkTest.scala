package klk

import scala.util.control.NonFatal

import cats.{Functor, MonadError}
import cats.effect.{Bracket, Resource}

case class KlkTest[F[_], Res, A](desc: String, thunk: TestReporter[F] => Res => F[KlkResult[A]])

object KlkTest
{
  def runPlain[F[_]: Functor, FR, A]
  (test: KlkTest[F, Unit, A])
  (fwRes: FR)
  (implicit compute: Compute[F], fw: TestFramework[F, FR])
  : KlkResult[A] =
    compute.run(test.thunk(fw.reporter(fwRes))(()))

  def runResource[F[_]: Bracket[*[_], Throwable], Res, FR, A]
  (resource: Resource[F, Res])
  (tests: collection.Seq[KlkTest[F, Res, A]])
  (fwRes: FR)
  (implicit compute: Compute[F], fw: TestFramework[F, FR])
  : KlkResult[A] =
    compute.run(resource.use(r => tests.toList.traverse(_.thunk(fw.reporter(fwRes))(r))).map(_.combineAll))

  def executeThunk[RunF[_]: MonadError[*[_], Throwable], SharedRes, FR, A]
  (desc: String)
  (thunk: SharedRes => RunF[KlkResult[A]])
  (reporter: TestReporter[RunF])
  (sharedRes: SharedRes)
  : RunF[KlkResult[A]] =
    for {
      testResult <- thunk(sharedRes)
        .recover { case NonFatal(e) => KlkResult.Fatal(e) }
        _ <- TestReporter.report[RunF, A](reporter)(desc)(testResult)
    } yield testResult

  def cons[RunF[_]: MonadError[*[_], Throwable], Res, A]
  (desc: String)
  (thunk: Res => RunF[KlkResult[A]])
  : KlkTest[RunF, Res, A] =
    KlkTest(desc, KlkTest.executeThunk(desc)(thunk))

  def plain[RunF[_]: MonadError[*[_], Throwable], A](desc: String)(thunk: RunF[KlkResult[A]]): KlkTest[RunF, Unit, A] =
    cons(desc)(_ => thunk)
}
