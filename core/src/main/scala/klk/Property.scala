package klk

import cats.Functor
import cats.data.Kleisli
import cats.effect.{Concurrent, Resource, Sync}
import cats.implicits._
import fs2.{Pull, Stream}
import fs2.concurrent.SignallingRef
import org.scalacheck.{Arbitrary, ForAll, Gen, Prop, Test => PropTest}
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

  def resultDetails: PropertyTestResult => KlkResultDetails[Boolean, Boolean] = {
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

  def run[F[_]: Sync]
  (concurrent: Resource[F, Concurrent[F]])
  (params: ScalacheckParams)
  (test: PropertyTest[F])
  : F[PropertyTestResult] =
    concurrent.use { implicit c =>
      stream(params)(test).compile.last.map(_.getOrElse(PropertyTestResult.noInput))
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
  def thunk(runner: PropGen.Runner[F])(f: Thunk): PropertyTest[F]
}

object PropGen
{
  trait Runner[F[_]]
  {
    def apply[A]: (A => PropertyTest[F]) => Arbitrary[A] => (A => Pretty) => PropertyTest[F]
  }

  object Runner
  {
    case class NoShrink[F[_]]()(implicit sync: Sync[F])
    extends Runner[F]
    {
      def apply[A]: (A => PropertyTest[F]) => Arbitrary[A] => (A => Pretty) => PropertyTest[F] =
        f => implicit arb => implicit pp => ForAll.noShrink(f)
    }
  }

  implicit def PropGen_Output[F[_]: Functor, P](implicit pv: P => Prop): PropGen[F, F[P]] =
    new PropGen[F, F[P]] {
      def thunk(runner: PropGen.Runner[F])(f: F[P]): PropertyTest[F] =
        PropertyTest(Kleisli(params => f.map(p => pv(p)(params))))
    }

  implicit def PropGen_f[F[_]: Sync, Thunk, P]
  (implicit next: PropGen[F, Thunk], arb: Arbitrary[P], pp: (P => Pretty))
  : PropGen[F, P => Thunk] =
    new PropGen[F, P => Thunk] {
      def thunk(runner: PropGen.Runner[F])(f: P => Thunk): PropertyTest[F] =
        runner.apply((p: P) => next.thunk(runner)(f(p)))(arb)(pp)
    }

  def apply[F[_]: Sync, Thunk]
  (concurrent: Resource[F, Concurrent[F]])
  (runner: Runner[F])
  (thunk: Thunk)
  (implicit propGen: PropGen[F, Thunk])
  : TestFunction[F, PropertyTestResult] =
    TestFunction(PropertyTest.run(concurrent)(ScalacheckParams.default)(propGen.thunk(runner)(thunk)))

  def noShrink[F[_]: Sync, Thunk]
  (concurrent: Resource[F, Concurrent[F]])
  (thunk: Thunk)
  (implicit propGen: PropGen[F, Thunk])
  : TestFunction[F, PropertyTestResult] =
    apply(concurrent)(Runner.NoShrink())(thunk)
}
