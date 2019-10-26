package klk

import cats.{Alternative, Applicative, Functor, Monad, StackSafeMonad}
import cats.data.{Kleisli, NonEmptyList, WriterT}
import cats.effect.{Bracket, Resource}
import cats.kernel.Monoid

case class TestStats(desc: String, result: KlkResult, duration: Long)

case class RunTestResources[F[_]](reporter: TestReporter[F], measure: MeasureTest[F])

object RunTestResources
{
  def cons[F[_], FR](res: FR)
  (implicit framework: TestFramework[F, FR], measure: MeasureTest[F])
  : RunTestResources[F] =
    RunTestResources(framework.reporter(res), measure)
}

sealed trait TestAlg[F[_], Res, +A]

// TODO could make the final result of a test be determined by the Output instead of the existence of a failure in the
// KlkResults. this would take care of the problem of having a failure result due to Unless
object TestAlg
{
  sealed trait Output[+A]

  object Output
  {
    case class Value[A](a: A)
    extends Output[A]

    case class Test[A](stats: List[TestStats], output: Output[A])
    extends Output[A]

    case object Zero
    extends Output[Nothing]

    case object Failure
    extends Output[Nothing]

    implicit def Monoid_Output[A]: Monoid[Output[A]] =
      new Monoid[Output[A]] {
        def combine(x: Output[A], y: Output[A]): Output[A] =
          (x, y) match {
            case (Failure, _) | (_, Failure) => Failure
            case (Value(a), Zero) => Value(a)
            case (Zero, Value(a)) => Value(a)
            case (a, _) => a
          }

        def empty: Output[A] =
          Zero
      }

    def unit: Output[Unit] =
      Value(())
  }

  case class Pure[F[_], Res, A](a: Output[A])
  extends TestAlg[F, Res, A]

  case class Single[F[_], Res, A](test: Res => RunTestResources[F] => F[Output[A]])
  extends TestAlg[F, Res, A]

  // TODO shared resources could be monadic. the extracted value is the resource and it is allocated when bound and
  // freed at the end of the test (probably won't work because of the lack of Bracket for WriterT)
  case class SharedResource[F[_], Res, A](run: RunTestResources[F] => F[Output[A]])
  extends TestAlg[F, Res, A]

  case class If[F[_], Res, A, B](head: TestAlg[F, Res, A], tail: A => TestAlg[F, Res, B])
  extends TestAlg[F, Res, B]

  case class Unless[F[_], Res, A](head: TestAlg[F, Res, A], tail: TestAlg[F, Res, A])
  extends TestAlg[F, Res, A]

  case class Sequential[F[_], Res, A](head: TestAlg[F, Res, A], tail: TestAlg[F, Res, A])
  extends TestAlg[F, Res, A]

  case class Parallel[F[_], Res, A](tests: NonEmptyList[TestAlg[F, Res, A]])(implicit val parallel: cats.Parallel[F])
  extends TestAlg[F, Res, A]

  case class Env[F[_], Res](reporter: TestReporter[F], res: Res)

  def testOutput(result: KlkResult): Output[Unit] =
    if (KlkResult.successful(result)) Output.unit
    else Output.Failure

  def runTest[F[_]: Functor, Res]
  (resource: Res)
  (test: KlkTest[F, Res])
  (testRes: RunTestResources[F])
  : F[Output[Unit]] =
    testRes.measure(test.thunk(testRes.reporter)(resource))
      .map {
        case (result, duration) =>
          Output.Test(List(TestStats(test.desc, result, duration)), testOutput(result))
      }


  def single[F[_]: Functor, Res](test: KlkTest[F, Res]): TestAlg[F, Res, Unit] =
    Single(res => testRes => runTest(res)(test)(testRes))

  def apply[F[_]: Functor, Res](tests: List[KlkTest[F, Res]]): TestAlg[F, Res, Unit] =
    tests match {
      case Nil => Pure(Output.Zero)
      case head :: tail => Sequential(single(head), apply(tail))
    }

  def unit[F[_], Res]: TestAlg[F, Res, Unit] =
    Pure(Output.unit)

  def bracketWriter[F[_]: Functor, A](thunk: RunTestAlg.W[F, Output[A]]): F[Output[A]] =
    thunk.run.map { case (stats, output) => Output.Test(stats, output) }

  def resource[F[_]: Bracket[*[_], Throwable], R, A]
  (resource: Resource[F, R], thunk: TestAlg[F, R, A])
  : TestAlg[F, Unit, A] =
    SharedResource(testRes => resource.use(r => bracketWriter(RunTestAlg.eval(r)(thunk).run(testRes))))

  def parallel[F[_], Res, A](head: TestAlg[F, Res, A], tail: TestAlg[F, Res, A]*)
  (implicit parallel: cats.Parallel[F])
  : TestAlg[F, Res, Unit] =
    Parallel(NonEmptyList(head.void, tail.toList.map(_.void)))(parallel)

  def sequential[F[_], Res, A](head: TestAlg[F, Res, A], tail: TestAlg[F, Res, A]*): TestAlg[F, Res, A] =
    tail.toList.foldRight(head)(Sequential(_, _))

  def depend[F[_], Res](head: TestAlg[F, Res, Unit])(tail: TestAlg[F, Res, Unit]): TestAlg[F, Res, Unit] =
    If[F, Res, Unit, Unit](head, _ => tail)

  def unless[F[_], Res](head: TestAlg[F, Res, Unit])(tail: TestAlg[F, Res, Unit]): TestAlg[F, Res, Unit] =
    Unless(head, tail)

  implicit def Instances_TestAlg[F[_], Res]
  : Monad[TestAlg[F, Res, *]] with Alternative[TestAlg[F, Res, *]] =
    new TestAlgInstances[F, Res]
}

class TestAlgInstances[F[_], Res]
extends StackSafeMonad[TestAlg[F, Res, *]]
with Alternative[TestAlg[F, Res, *]]
{
  def flatMap[A, B](fa: TestAlg[F, Res, A])(f: A => TestAlg[F, Res, B]): TestAlg[F, Res, B] =
    TestAlg.If(fa, f)

  def pure[A](a: A): TestAlg[F, Res, A] =
    TestAlg.Pure(TestAlg.Output.Value(a))

  def combineK[A](x: TestAlg[F, Res, A], y: TestAlg[F, Res, A]): TestAlg[F, Res, A] =
    TestAlg.Unless(x, y)

  def empty[A]: TestAlg[F, Res, A] =
    TestAlg.Pure(TestAlg.Output.Zero)
}

object RunTestAlg
{
  type W[F[_], A] = WriterT[F, List[TestStats], A]
  type M[F[_], A] = Kleisli[W[F, *], RunTestResources[F], A]

  def liftW[F[_]: Applicative, A](fa: F[A]): W[F, A] =
    WriterT.liftF(fa)

  def lift[F[_]: Applicative, Res, A](fa: F[A]): M[F, A] =
    Kleisli.liftF(liftW(fa))

  def tell[F[_]: Applicative, Res](result: List[TestStats]): M[F, Unit] =
    Kleisli.liftF(WriterT.tell(result))

  def tell1[F[_]: Applicative, Res](result: TestStats): M[F, Unit] =
    tell(List(result))

  def fail[F[_]: Applicative, Res, A]: M[F, TestAlg.Output[A]] =
    Kleisli.pure(TestAlg.Output.Failure)

  def unit[F[_]: Applicative]: M[F, Unit] =
    Kleisli.pure(())

  def eval1[F[_]: Monad, Res, A]
  (res: Res)
  : TestAlg[F, Res, A] => M[F, TestAlg.Output[A]] = {
    case TestAlg.Pure(a) =>
      Kleisli.pure(a)
    case TestAlg.Single(test) =>
      Kleisli(r => liftW(test(res)(r).widen))
    case TestAlg.SharedResource(run) =>
      Kleisli(r => liftW(run(r).widen))
    case TestAlg.If(head, tail) =>
      eval(res)(head)
        .flatMap {
          case TestAlg.Output.Value(a) => eval(res)(tail(a))
          case _ => fail
        }
    case TestAlg.Unless(head, tail) =>
      eval(res)(head)
        .flatMap {
          case TestAlg.Output.Failure => eval(res)(tail)
          case a => Kleisli.pure(a)
        }
    case TestAlg.Sequential(head, tail) =>
      eval(res)(head) *> eval(res)(tail)
    case ta @ TestAlg.Parallel(tests) =>
      import ta.parallel
      tests.parTraverse(a => eval(res)(a)).map(_.combineAll)
  }

  def handleOutput[F[_]: Applicative, A]: TestAlg.Output[A] => M[F, TestAlg.Output[A]] = {
    case TestAlg.Output.Test(stats, output) => tell[F, RunTestResources[F]](stats) *> Kleisli.pure(output)
    case a => Kleisli.pure(a)
  }

  def eval[F[_]: Monad, Res, A]
  (res: Res)
  (test: TestAlg[F, Res, A])
  : M[F, TestAlg.Output[A]] =
    for {
      output <- eval1[F, Res, A](res).apply(test)
      newOutput <- handleOutput[F, A].apply(output)
    } yield newOutput

  def run[F[_]: Monad, A]
  (tests: TestAlg[F, Unit, A])
  : Kleisli[F, RunTestResources[F], List[TestStats]] =
    eval(())(tests).mapF(_.written)
}
