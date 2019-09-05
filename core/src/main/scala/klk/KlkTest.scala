package klk

import cats.Functor
import cats.effect.{Bracket, Resource}
import cats.implicits._

case class FinalResult()

case class KlkTest[F[_], R](desc: String, thunk: TestLog => R => F[FinalResult])

object KlkTest
{
  def runPlain[F[_]: Functor]
  (log: TestLog)
  (effect: TestEffect[F])
  (test: KlkTest[F, Unit])
  : FinalResult =
    effect.run(test.thunk(log)(()).as(FinalResult()))

  def runResource[F[_]: Bracket[?[_], Throwable], R]
  (log: TestLog)
  (effect: TestEffect[F])
  (resource: Resource[F, R])
  (tests: List[KlkTest[F, R]])
  : FinalResult =
    effect.run(resource.use(r => tests.traverse_(_.thunk(log)(r))).as(FinalResult()))
}
