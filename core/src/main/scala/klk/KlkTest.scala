package klk

import cats.Functor
import cats.effect.{Bracket, Resource}
import cats.implicits._

case class KlkTest[F[_], R](desc: String, thunk: TestReporter[F] => R => F[KlkResult])

object KlkTest
{
  def runPlain[F[_]: Functor, FR]
  (test: KlkTest[F, Unit])
  (fwRes: FR)
  (implicit compute: Compute[F], fw: TestFramework[F, FR])
  : KlkResult =
    compute.run(test.thunk(fw.reporter(fwRes))(()))

  def runResource[F[_]: Bracket[*[_], Throwable], R, FR]
  (resource: Resource[F, R])
  (tests: collection.Seq[KlkTest[F, R]])
  (fwRes: FR)
  (implicit compute: Compute[F], fw: TestFramework[F, FR])
  : KlkResult =
    compute.run(resource.use(r => tests.toList.traverse(_.thunk(fw.reporter(fwRes))(r))).map(_.combineAll))
}

case class TestThunk[FR](desc: String, thunk: FR => KlkResult)
