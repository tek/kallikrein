package klk

import cats.{Functor, Monad, Reducible}
import cats.data.{Kleisli, OptionT, WriterT}

object EvalSuite
{
  type State[A] = (List[TestStats], Option[A])
  type K[F[_], A] = Kleisli[F, RunTestResources[F], A]
  type M[F[_], A] = OptionT[WriterT[K[F, *], List[TestStats], *], A]

  def lift[F[_], A](fa: K[F, State[A]]): M[F, A] =
    OptionT(WriterT(fa))

  def unless[F[_]: Monad, A](condition: M[F, A])(alternative: M[F, A]): M[F, A] =
    lift(
      condition.value.run.flatMap {
        case (stats, Some(a)) =>
          Kleisli.pure((stats, Some(a)))
        case (stats, None) =>
          alternative.value.run.map {
            case (newStats, Some(a)) =>
              (stats.map(TestStats.recover) ++ newStats, Some(a))
            case (newStats, None) =>
              (newStats ++ stats, None)
          }
      }
  )

  def liftTest[F[_]: Monad, A](fa: K[F, Suite.Output[A]]): M[F, A] =
    lift(fa.flatMapF(output => (output.stats, Suite.Output.toOption(output)).pure[F]))

  def combineResults[A](x: State[A], y: State[A]): State[A] =
    (x, y) match {
      case ((statsX, valueX), (statsY, valueY)) =>
        (statsX |+| statsY, valueX <* valueY)
    }

  def foldSuites[F[_]: Functor, T[_]: Reducible, A](fa: F[T[State[A]]]): F[State[A]] =
    fa.map(_.reduceLeft(combineResults))

  def step[F[_]: Monad, Res, A]
  (res: Res)
  : Suite[F, Res, A] => M[F, A] = {
    case Suite.Pure(a) =>
      OptionT.pure(a)
    case Suite.Suspend(test) =>
      liftTest(Kleisli(test(res))).widen
    case suite @ Suite.SharedResource(resource, test) =>
      import suite.bracket
      lift(Kleisli(testRes => resource.use(r => evalF(r)(test).run(testRes))))
    case Suite.If(head, tail) =>
      eval(res)(head) >>= tail.andThen(eval(res))
    case Suite.Unless(head, tail) =>
      unless(eval(res)(head))(eval(res)(tail))
    case Suite.Sequential(tests) =>
      OptionT(WriterT(foldSuites(tests.traverse(evalF(res)))))
    case suite @ Suite.Parallel(tests) =>
      import suite.parallel
      tests.parTraverse(eval(res)).map(_.head)
  }

  def eval[F[_]: Monad, Res, A]
  (res: Res)
  (test: Suite[F, Res, A])
  : M[F, A] =
    step[F, Res, A](res).apply(test)

  def evalF[F[_]: Monad, Res, A]
  (res: Res)
  (test: Suite[F, Res, A])
  : K[F, State[A]] =
    eval(res)(test).value.run

  def apply[F[_]: Monad, A]
  (tests: Suite[F, Unit, A])
  : Kleisli[F, RunTestResources[F], List[TestStats]] =
    eval(())(tests).value.written
}
