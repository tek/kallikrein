package klk

import cats.{Foldable, Functor, Monoid}
import cats.effect.{Bracket, Resource}
import cats.implicits._

case class FinalResult()

object FinalResult
{
  implicit def Monoid_FinalResult: Monoid[FinalResult] =
    new Monoid[FinalResult] {
      def empty: FinalResult =
        FinalResult()

      def combine(a: FinalResult, b: FinalResult): FinalResult =
        FinalResult()
    }
}

case class KlkTest[F[_], R](desc: String, thunk: TestLog => R => F[FinalResult])

object KlkTest
{
  def runPlain[F[_]: Functor]
  (log: TestLog)
  (effect: Compute[F])
  (test: KlkTest[F, Unit])
  : FinalResult =
    effect.run(test.thunk(log)(()))

  def runResource[F[_]: Bracket[?[_], Throwable], R]
  (log: TestLog)
  (effect: Compute[F])
  (resource: Resource[F, R])
  (tests: List[KlkTest[F, R]])
  : FinalResult =
    effect.run(resource.use(r => tests.traverse(_.thunk(log)(r))).map(Foldable[List].fold[FinalResult]))
}
