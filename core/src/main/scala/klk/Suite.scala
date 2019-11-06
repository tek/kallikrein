package klk

import cats.{Functor, Monad, SemigroupK, StackSafeMonad}
import cats.data.NonEmptyList
import cats.effect.{Bracket, Resource}
import cats.kernel.Monoid

case class TestStats(
  desc: String,
  result: List[KlkResult.Details],
  success: Boolean,
  recovered: Boolean,
  duration: Long,
)

object TestStats
{
  def recover(stats: TestStats): TestStats =
    stats.copy(recovered = true)

  def reportAsSuccess(stats: TestStats): Boolean =
    stats.success || stats.recovered
}

case class RunTestResources[F[_]](reporter: TestReporter[F], measure: MeasureTest[F])

object RunTestResources
{
  final class RTRCons[F[_]] {
    def apply[FR](res: FR)
    (implicit framework: TestFramework[F, FR], measure: MeasureTest[F])
    : RunTestResources[F] =
      RunTestResources(framework.reporter(res), measure)
  }

  def cons[F[_]]: RTRCons[F] =
    new RTRCons[F]
}

sealed trait Suite[F[_], Res, +A]

object Suite
{
  case class Output[+A](details: Output.Details[A], continue: Boolean, stats: List[TestStats])

  object Output
  {
    sealed trait Details[+A]

    object Details
    {
      case class Value[A](a: A)
      extends Details[A]

      case object Zero
      extends Details[Nothing]

      def zero[A]: Details[A] =
        Zero

      def pure[A](a: A): Details[A] =
        Value(a)

      implicit def Monoid_Details[A]: Monoid[Details[A]] =
        new Monoid[Details[A]] {
          def combine(x: Details[A], y: Details[A]): Details[A] =
            (x, y) match {
              case (_, Value(a)) => Value(a)
              case (Value(a), Zero) => Value(a)
              case (Zero, Zero) => Zero
            }

          def empty: Details[A] =
            zero
        }

      def fromResult[A]: KlkResult[A] => Details[A] = {
        case KlkResult.Value(a) => Value(a)
        case _ => Zero
      }

      def fromOption[A]: Option[A] => Details[A] = {
        case Some(a) => Value(a)
        case None => Zero
      }

      def successful[A]: Details[A] => Boolean = {
        case Value(_) => true
        case Zero => false
      }
    }

    def empty[A]: Output[A] =
      Output(Details.Zero, true, Nil)

    def pure[A](a: A): Output[A] =
      Output(Details.Value(a), true, Nil)

    implicit def Monoid_Output[A]: Monoid[Output[A]] =
      new Monoid[Output[A]] {
        def combine(x: Output[A], y: Output[A]): Output[A] =
          Output(x.details |+| y.details, x.continue && y.continue, x.stats |+| y.stats)

        def empty: Output[A] =
          Output.empty
      }

    object Value
    {
      def unapply[A](output: Output[A]): Option[A] =
        output.details match {
          case Details.Value(a) => Some(a)
          case _ => None
        }
    }

    def toOption[A]: Output[A] => Option[A] = {
      case Output(Details.Value(a), true, _) => Some(a)
      case _ => None
    }
  }

  case class Pure[F[_], Res, A](a: A)
  extends Suite[F, Res, A]

  case class Suspend[F[_], Res, A](test: Res => RunTestResources[F] => F[Output[A]])
  extends Suite[F, Res, A]

  case class SharedResource[F[_], Res, R, A](resource: Resource[F, R], suite: Suite[F, R, A])(
    implicit val bracket: Bracket[F, Throwable])
  extends Suite[F, Res, A]

  case class If[F[_], Res, A, B](head: Suite[F, Res, A], tail: A => Suite[F, Res, B])
  extends Suite[F, Res, B]

  case class Unless[F[_], Res, A](head: Suite[F, Res, A], tail: Suite[F, Res, A])
  extends Suite[F, Res, A]

  case class Sequential[F[_], Res, A](tests: NonEmptyList[Suite[F, Res, A]])
  extends Suite[F, Res, A]

  case class Parallel[F[_], Res, A](tests: NonEmptyList[Suite[F, Res, A]])(implicit val parallel: cats.Parallel[F])
  extends Suite[F, Res, A]

  def runTest[F[_]: Functor, Res, A]
  (resource: Res)
  (test: KlkTest[F, Res, A])
  (testRes: RunTestResources[F])
  : F[Output[A]] =
    testRes.measure(test.thunk(testRes.reporter)(resource))
      .map {
        case (result, duration) =>
          val success = KlkResult.successful(result)
          Output(
            Output.Details.fromResult(result),
            success,
            List(TestStats(test.desc, KlkResult.details(result), success, false, duration)),
          )
      }

  def single[F[_]: Functor, Res, A](test: KlkTest[F, Res, A]): Suite[F, Res, A] =
    Suspend(res => testRes => runTest(res)(test)(testRes))

  def resource[F[_], R, A]
  (resource: Resource[F, R], thunk: Suite[F, R, A])
  (implicit bracket: Bracket[F, Throwable])
  : Suite[F, Unit, A] =
    SharedResource(resource, thunk)

  def parallel[F[_]: cats.Parallel, Res, A](head: Suite[F, Res, A], tail: Suite[F, Res, A]*)
  : Suite[F, Res, A] =
    Parallel(NonEmptyList(head, tail.toList))

  def sequential[F[_], Res, A](head: Suite[F, Res, A], tail: Suite[F, Res, A]*): Suite[F, Res, A] =
    Sequential(NonEmptyList(head, tail.toList))

  def depend[F[_], Res](head: Suite[F, Res, Unit])(tail: Suite[F, Res, Unit]): Suite[F, Res, Unit] =
    If[F, Res, Unit, Unit](head, _ => tail)

  def unless[F[_], Res](head: Suite[F, Res, Unit])(tail: Suite[F, Res, Unit]): Suite[F, Res, Unit] =
    Unless(head, tail)

  implicit def Instances_Suite[F[_], Res]
  : Monad[Suite[F, Res, *]] with SemigroupK[Suite[F, Res, *]] =
    new SuiteInstances[F, Res]
}

class SuiteInstances[F[_], Res]
extends StackSafeMonad[Suite[F, Res, *]]
with SemigroupK[Suite[F, Res, *]]
{
  def flatMap[A, B](fa: Suite[F, Res, A])(f: A => Suite[F, Res, B]): Suite[F, Res, B] =
    Suite.If(fa, f)

  def pure[A](a: A): Suite[F, Res, A] =
    Suite.Pure(a)

  def combineK[A](x: Suite[F, Res, A], y: Suite[F, Res, A]): Suite[F, Res, A] =
    Suite.Unless(x, y)
}
