package klk

import cats.{Functor, Monad}
import cats.data.Kleisli
import cats.effect.{Concurrent, Resource, Sync}
import cats.implicits._
import fs2.{Pull, Stream}
import fs2.concurrent.SignallingRef
import org.scalacheck.{Arbitrary, ForAll, Gen, Prop, Test => PropTest}
import org.scalacheck.Test.{Parameters => TestParameters}
import org.scalacheck.util.{FreqMap, Pretty}

case class ScalacheckParams(test: TestParameters, gen: Gen.Parameters, sizeStep: Int)

object ScalacheckParams
{
  def cons(test: TestParameters, gen: Gen.Parameters): ScalacheckParams = {
    val iterations = math.ceil(test.minSuccessfulTests / test.workers.toDouble)
    val sizeStep = math.round((test.maxSize - test.minSize) / (iterations * test.workers)).toInt
    ScalacheckParams(test, gen, sizeStep)
  }

  def default: ScalacheckParams =
    cons(TestParameters.default, Gen.Parameters.default)
}

case class PropertyTestState(finished: Boolean, discarded: Int)

case class PropertyTestResult(success: Boolean, result: PropTest.Result)

case class PropertyTest[F[_]](test: Kleisli[F, Gen.Parameters, Prop.Result])

object PropertyTest
{
  def single[F[_]: Monad]
  (terminate: SignallingRef[F, Boolean])
  (test: PropertyTest[F])
  (params: Gen.Parameters)
  : F[Prop.Result] =
    for {
      result <- test.test.run(params)
      _ <- terminate.set(true).whenA(PropResult.finished(result))
    } yield result

  def updateState(propResult: Prop.Result): PropertyTestState => PropertyTestState = {
    case PropertyTestState(finished, discarded) =>
      propResult.status match {
        case Prop.True => PropertyTestState(finished, discarded)
        case Prop.False => PropertyTestState(true, discarded)
        case Prop.Proof => PropertyTestState(true, discarded)
        case Prop.Undecided => PropertyTestState(false, discarded)
        case Prop.Exception(_) => PropertyTestState(true, discarded)
      }
  }

  def finish[F[_]]: PropertyTestState => Pull[F, PropertyTestResult, Unit] = {
    case PropertyTestState(_, discarded) =>
    Pull.output1(PropertyTestResult(true, PropTest.Result(PropTest.Passed, 0, discarded, FreqMap.empty))) *> Pull.done
  }

  def aggregateResults[F[_]]
  (terminate: SignallingRef[F, Boolean])
  (params: ScalacheckParams)
  (state: PropertyTestState)
  (in: Stream[F, Prop.Result])
  : Pull[F, PropertyTestResult, Unit] =
    in.pull.uncons1.flatMap {
      case Some((propResult, rest)) =>
        val updated = updateState(propResult)(state)
        if (updated.finished) Pull.eval(terminate.set(true)) >> finish[F](updated)
        else aggregateResults(terminate)(params)(updated)(rest)
      case None =>
        finish[F](state)
    }

  def concurrent[F[_]: Concurrent]
  (terminate: SignallingRef[F, Boolean])
  (params: ScalacheckParams)
  (test: PropertyTest[F])
  : Stream[F, PropertyTestResult] =
    Stream.range(params.test.minSize, params.test.maxSize, params.sizeStep)
      .interruptWhen(terminate)
      .map(params.gen.withSize)
      .map(single(terminate)(test))
      .map(Stream.eval)
      .parJoin(params.test.workers)
      .through(in => aggregateResults(terminate)(params)(PropertyTestState(false, 0))(in).stream)

  def stream[F[_]: Concurrent]
  (params: ScalacheckParams)
  (test: PropertyTest[F])
  : Stream[F, PropertyTestResult] =
    for {
      terminate <- Stream.eval(SignallingRef(false))
      result <- concurrent(terminate)(params)(test)
    } yield result

  def run[F[_]: Sync]
  (concurrent: Resource[F, Concurrent[F]])
  (params: ScalacheckParams)
  (test: PropertyTest[F])
  : F[PropertyTestResult] =
    concurrent.use { implicit c =>
      stream(params)(test).compile.last.map(_.getOrElse(PropertyTestResult(false, ???)))
    }
}

object PropStatus
{
  def bool: Boolean => Prop.Status = {
    case true => Prop.True
    case false => Prop.False
  }

  def finished: Prop.Status => Boolean = {
    case Prop.Exception(_) => true
    case Prop.Proof | Prop.False => true
    case _ => false
  }
}

object PropResult
{
  def bool(a: Boolean): Prop.Result =
    Prop.Result(PropStatus.bool(a))

  def finished(result: Prop.Result): Boolean =
    PropStatus.finished(result.status)
}

trait PropGen[F[_], Thunk]
{
  def thunk(f: Thunk): PropertyTest[F]
}

object PropGen
{
  implicit def PropGen_Output[F[_]: Functor, P](implicit pv: P => Prop): PropGen[F, F[P]] =
    new PropGen[F, F[P]] {
      def thunk(f: F[P]): PropertyTest[F] =
        PropertyTest(Kleisli(params => f.map(p => pv(p)(params))))
    }

  implicit def PropGen_f[F[_]: Sync, Thunk, P: Arbitrary]
  (implicit next: PropGen[F, Thunk], pp1: P => Pretty)
  : PropGen[F, P => Thunk] =
    new PropGen[F, P => Thunk] {
      def thunk(f: P => Thunk): PropertyTest[F] =
        ForAll.noShrink((p: P) => next.thunk(f(p)))
    }

  def apply[F[_]: Sync, Thunk]
  (concurrent: Resource[F, Concurrent[F]])
  (thunk: Thunk)
  (implicit gen: PropGen[F, Thunk])
  : TestFunction[F, PropertyTestResult] =
    TestFunction(PropertyTest.run(concurrent)(ScalacheckParams.default)(gen.thunk(thunk)))
}
