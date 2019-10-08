package klk

import java.util.concurrent.{ExecutorService, ThreadPoolExecutor}

import cats.{Applicative, Functor}
import cats.data.Kleisli
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import fs2.{Pull, Stream}
import fs2.concurrent.SignallingRef
import org.{scalacheck => sc}
import org.scalacheck.{Arbitrary, ForAllNoShrink, ForAllShrink, Gen, Prop, Test => PropTest}
import org.scalacheck.Test.{Parameters => TestParameters}
import org.scalacheck.util.{FreqMap, Pretty}

case class ScalacheckParams(test: TestParameters, gen: Gen.Parameters, sizeStep: Int, maxDiscarded: Float)

object ScalacheckParams
{
  def cons(test: TestParameters, gen: Gen.Parameters): ScalacheckParams = {
    val iterations = math.ceil(test.minSuccessfulTests / test.workers.toDouble)
    val sizeStep = math.round((test.maxSize - test.minSize) / (iterations * test.workers)).toInt
    val maxDiscarded = test.maxDiscardRatio * test.minSuccessfulTests
    ScalacheckParams(test, gen, sizeStep, maxDiscarded)
  }

  def default: ScalacheckParams =
    cons(TestParameters.default, Gen.Parameters.default)
}

case class PropertyTestState(stats: PropertyTestState.Stats, result: PropTest.Status)

object PropertyTestState
{
  case class Stats(finished: Boolean, iterations: Int, discarded: Int)

  object Stats
  {
    def zero: Stats =
      Stats(false, 0, 0)
  }

  def updateStats(status: Prop.Status, maxDiscarded: Float): Stats => Stats = {
    case Stats(finished, iterations, discarded) =>
      val (newFinished, newDiscarded) =
        status match {
          case Prop.True =>
            (finished, discarded)
          case Prop.False =>
            (true, discarded)
          case Prop.Proof =>
            (true, discarded)
          case Prop.Undecided =>
            (discarded + 1 > maxDiscarded, discarded + 1)
          case Prop.Exception(_) =>
            (true, discarded)
        }
      Stats(newFinished, iterations + 1, newDiscarded)
  }

  def updateResult
  (params: ScalacheckParams, discarded: Int)
  (current: PropTest.Status)
  (propResult: Prop.Result)
  : Prop.Status => PropTest.Status = {
    case Prop.True =>
      current
    case Prop.False =>
      PropTest.Failed(propResult.args, propResult.labels)
    case Prop.Proof =>
      PropTest.Proved(propResult.args)
    case Prop.Undecided if discarded + 1 > params.maxDiscarded =>
      PropTest.Exhausted
    case Prop.Undecided =>
      current
    case Prop.Exception(e) =>
      PropTest.PropException(propResult.args, e, propResult.labels)
  }

  def update(params: ScalacheckParams)(propResult: Prop.Result): PropertyTestState => PropertyTestState = {
    case PropertyTestState(stats, result) =>
      PropertyTestState(
        updateStats(propResult.status, params.maxDiscarded)(stats),
        updateResult(params, stats.discarded)(result)(propResult)(propResult.status),
      )
  }

  def zero: PropertyTestState =
    PropertyTestState(Stats.zero, PropTest.Passed)
}

case class PropertyTestResult(success: Boolean, stats: PropertyTestState.Stats, result: PropTest.Result)

object PropertyTestResult
{
  def noInput: PropertyTestResult =
    PropertyTestResult(false, PropertyTestState.Stats.zero, PropTest.Result(PropTest.Exhausted, 0, 0, FreqMap.empty))

  def resultDetails: PropertyTestResult => KlkResultDetails = {
    case PropertyTestResult(_, PropertyTestState.Stats(_, iterations, discarded), result) =>
      val message: List[String] = result.status match {
        case PropTest.Exhausted => List(s"exhausted after $iterations iterations, discarding $discarded")
        case PropTest.Passed => List(s"passed after $iterations iterations")
        case PropTest.Proved(_) => List(s"proved after $iterations iterations")
        case PropTest.Failed(args, labels) =>
          s"failed after $iterations iterations for" :: args.map(_.toString) ::: labels.toList
        case PropTest.PropException(args, e, labels) =>
          val exception = e.getMessage :: e.getStackTrace.toList.map(_.toString)
          "failed with exception" :: args.map(_.toString) ::: labels.toList ::: exception
      }
      KlkResultDetails.Simple(message)
  }

  def success: PropTest.Status => Boolean = {
    case PropTest.Exhausted => false
    case PropTest.Failed(_, _) => false
    case PropTest.PropException(_, _, _) => false
    case PropTest.Proved(_) => true
    case PropTest.Passed => true
  }
}

case class PropertyTestOutput[Trans](result: PropertyTestResult)

case class PropertyTest[F[_]](test: Kleisli[F, Gen.Parameters, Prop.Result])

object PropertyTest
{
  def finish[F[_]]: PropertyTestState => Pull[F, PropertyTestResult, Unit] = {
    case PropertyTestState(stats @ PropertyTestState.Stats(_, iterations, discarded), status) =>
      val result = PropertyTestResult(
        PropertyTestResult.success(status),
        stats,
        PropTest.Result(status, iterations, discarded, FreqMap.empty),
      )
      Pull.output1(result) *> Pull.done
  }

  def aggregateResults[F[_]]
  (terminate: SignallingRef[F, Boolean])
  (params: ScalacheckParams)
  (state: PropertyTestState)
  (in: Stream[F, Prop.Result])
  : Pull[F, PropertyTestResult, Unit] =
    in.pull.uncons1.flatMap {
      case Some((propResult, rest)) =>
        val updated = PropertyTestState.update(params)(propResult)(state)
        if (updated.stats.finished) Pull.eval(terminate.set(true)) >> finish[F](updated)
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
      .map(test.test.run)
      .map(Stream.eval)
      .parJoin(params.test.workers)
      .through(in => aggregateResults(terminate)(params)(PropertyTestState.zero)(in).stream)

  def stream[F[_]: Concurrent]
  (params: ScalacheckParams)
  (test: PropertyTest[F])
  : Stream[F, PropertyTestResult] =
    for {
      terminate <- Stream.eval(SignallingRef(false))
      result <- concurrent(terminate)(params)(test)
    } yield result

  def discardingPool[F[_]: Sync](threads: Int): F[ExecutorService] =
    Concurrency.fixedPoolWith(threads).flatMap {
      case es: ThreadPoolExecutor =>
        Sync[F].delay(es.setRejectedExecutionHandler(new ThreadPoolExecutor.DiscardPolicy)).as(es)
      case es =>
        Sync[F].pure(es)
    }

  def run[F[_]: Sync]
  (concurrent: ConsConcurrent[F])
  (params: ScalacheckParams)
  (test: PropertyTest[F])
  : F[PropertyTestResult] =
    Concurrency.ec[F](discardingPool[F](params.test.workers))
      .map(concurrent.pool)
      .map(stream(params)(test)(_))
      .use(_.compile.last.map(_.getOrElse(PropertyTestResult.noInput)))

  type K[F[_], A] = Kleisli[F, Gen.Parameters, A]

  class PropertyTestKleisli[F[_]: Applicative]
  {
    def pure[A](a: A): K[F, A] =
      Kleisli.pure(a)
  }

  def kleisli[F[_]: Applicative]: PropertyTestKleisli[F] =
    new PropertyTestKleisli[F]
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

trait PropTrans[F[_], Trans, A]
{
  def apply(input: A => PropertyTest[F]): PropertyTest[F]
}

object PropTrans
{
  type Full
  type Shrink

  implicit def PropTrans_Full[F[_]: Sync, A: Arbitrary]
  (implicit pp1: A => Pretty)
  : PropTrans[F, Full, A] =
    new PropTrans[F, Full, A] {
      def apply(f: A => PropertyTest[F]): PropertyTest[F] =
        ForAllNoShrink(f)
    }

  implicit def PropTrans_Shrink[F[_]: Sync, A: sc.Shrink: Arbitrary]
  (implicit pp1: A => Pretty)
  : PropTrans[F, Shrink, A] =
    new PropTrans[F, Shrink, A] {
      def apply(f: A => PropertyTest[F]): PropertyTest[F] =
        ForAllShrink(f)
    }
}

trait PropThunk[Thunk, Trans]
{
  type F[A]

  def apply(thunk: Thunk): PropertyTest[F]
}

object PropThunk
{
  type Aux[Thunk, Trans, F0[_]] = PropThunk[Thunk, Trans] { type F[A] = F0[A] }

  implicit def PropThunk_Output[F0[_]: Functor, P, Trans]
  (implicit pv: P => Prop)
  : PropThunk.Aux[F0[P], Trans, F0] =
    new PropThunk[F0[P], Trans] {
      type F[A] = F0[A]
      def apply(f: F[P]): PropertyTest[F] =
        PropertyTest(Kleisli(params => f.map(p => pv(p)(params))))
    }

  implicit def PropThunk_f[F0[_], Thunk, P, Trans]
  (implicit next: PropThunk.Aux[Thunk, Trans, F0], trans: PropTrans[F0, Trans, P])
  : PropThunk.Aux[P => Thunk, Trans, F0] =
    new PropThunk[P => Thunk, Trans] {
      type F[A] = F0[A]
      def apply(f: P => Thunk): PropertyTest[F] =
        trans((p: P) => next(f(p)))
    }
}

trait PropRun[Thunk, Trans]
{
  type F[A]
  def apply(thunk: Thunk): PropertyTest[F]
  def sync: Sync[F]
  def pool: ConsConcurrent[F]
}

object PropRun
{
  type Aux[Thunk, Trans, F0[_]] = PropRun[Thunk, Trans] { type F[A] = F0[A] }

  implicit def PropRun_Any[Thunk, Trans, F0[_]]
  (implicit propThunk: PropThunk.Aux[Thunk, Trans, F0], syncF: Sync[F0], poolF: ConsConcurrent[F0])
  : PropRun.Aux[Thunk, Trans, F0] =
    new PropRun[Thunk, Trans] {
      type F[A] = F0[A]
      def apply(thunk: Thunk): PropertyTest[F] = propThunk(thunk)
      def sync: Sync[F] = syncF
      def pool: ConsConcurrent[F] = poolF
    }

  def apply[Thunk, Trans]
  (propRun: PropRun[Thunk, Trans])
  (thunk: Thunk)
  : propRun.F[PropertyTestOutput[Trans]] = {
    implicit def functor: Functor[propRun.F] = propRun.sync
    PropertyTest.run(propRun.pool)(ScalacheckParams.default)(propRun(thunk))(propRun.sync)
      .map(PropertyTestOutput(_))
  }
}
