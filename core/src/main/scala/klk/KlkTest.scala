package klk

import cats.{Foldable, Functor}
import cats.effect.{Bracket, Resource}
import cats.implicits._

case class KlkTest[F[_], R](desc: String, thunk: TestLog => R => F[KlkResult])

object KlkTest
{
  def runPlain[F[_]: Functor]
  (log: TestLog)
  (effect: Compute[F])
  (test: KlkTest[F, Unit])
  : KlkResult =
    effect.run(test.thunk(log)(()))

  def runResource[F[_]: Bracket[*[_], Throwable], R]
  (log: TestLog)
  (effect: Compute[F])
  (resource: Resource[F, R])
  (tests: List[KlkTest[F, R]])
  : KlkResult =
    effect.run(resource.use(r => tests.traverse(_.thunk(log)(r))).map(Foldable[List].fold[KlkResult]))
}
