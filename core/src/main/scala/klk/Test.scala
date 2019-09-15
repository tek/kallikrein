package klk

import scala.collection.mutable

import cats.effect.{Bracket, Resource, Sync}
import cats.implicits._
import shapeless.{::, HList, HNil}

sealed trait KlkTestSet

object KlkTestSet
{
  case class Plain[F[_]](test: KlkTest[F, Unit])
  extends KlkTestSet

  case class Resource[F[_], R](resource: SharedResource[F, R])
  extends KlkTestSet
}

// when executing shared resource tests, a single task will produce multiple results, since the acquisition of the
// resource cannot be split into unsafe tasks, hence the KlkTestSet
case class KlkTests()
{
  val tests: mutable.Buffer[TestThunk] =
    mutable.Buffer.empty

  def add(test: TestThunk): Unit =
    tests += test

  def plain[F[_]](test: KlkTest[F, Unit])(implicit effect: TestEffect[F]): Unit =
    add(TestThunk(log => KlkTest.runPlain(log)(effect)(test)(effect.sync)))

  def resource[F[_]: Bracket[?[_], Throwable], R](r: SharedResource[F, R]): Unit =
    add(TestThunk(log => KlkTest.runResource(log)(r.effect)(r.resource)(r.tests.toList)))
}

trait StripResources[R <: HList, ThunkF]
{
  type Thunk

  def apply(resources: TestResources[R])(thunk: ThunkF): Thunk
}

object StripResources
{
  type Aux[R <: HList, ThunkF, Thunk0] =
    StripResources[R, ThunkF] {
      type Thunk = Thunk0
    }

  implicit def StripResources_HList[F[_]: Bracket[?[_], Throwable], H, T <: HList, ThunkF, Output]
  (implicit next: StripResources.Aux[T, ThunkF, F[Output]])
  : Aux[Resource[F, H] :: T, H => ThunkF, F[Output]] =
    new StripResources[Resource[F, H] :: T, H => ThunkF] {
      type Thunk = F[Output]

      def apply(resources: TestResources[Resource[F, H] :: T])(thunk: H => ThunkF): F[Output] =
        resources.resources.head.use(h => next(TestResources(resources.resources.tail))(thunk(h)))
    }

  implicit def StripResources_HNil[F[_], Output]
  : Aux[HNil, F[Output], F[Output]] =
    new StripResources[HNil, F[Output]] {
      type Thunk = F[Output]

      def apply(resources: TestResources[HNil])(thunk: Thunk): Thunk =
        thunk
    }
}

case class TestResources[R <: HList](resources: R)

case class TestBuilder[F[_], RR <: HList]
(tests: KlkTests, desc: String)
(reporter: TestReporter)
(resources: TestResources[RR])
{
  def apply[Thunk, Thunk0, Output, Expected, Actual]
  (thunk: Thunk)
  (
    implicit
    strip: StripResources.Aux[RR, Thunk, F[Output]],
    result: TestResult[F, Output, Expected, Actual],
    effect: TestEffect[F],
  )
  : Unit =
    tests.plain(PlainTest(result, reporter)(desc)(TestFunction(strip(resources)(thunk)))(effect.sync))

  def forallNoShrink[Thunk, Thunk0, Expected, Actual]
  (thunk: Thunk)
  (
    implicit
    strip: StripResources.Aux[RR, Thunk, Thunk0],
    propGen: PropGen[F, Thunk0],
    result: TestResult[F, PropertyTestResult, Boolean, Boolean],
    effect: TestEffect[F],
  )
  : Unit =
    tests.plain(PlainTest.forallNoShrink(propGen, result, reporter, effect)(desc)(strip(resources)(thunk))(effect.sync))

  def resource[R](r: Resource[F, R]): TestBuilder[F, Resource[F, R] :: RR] =
    TestBuilder(tests, desc)(reporter)(TestResources(r :: resources.resources))
}

case class SharedResource[F[_], R]
(resource: Resource[F, R])
(reporter: TestReporter)
(implicit val effect: TestEffect[F])
{
  val tests: mutable.Buffer[KlkTest[F, R]] =
    mutable.Buffer.empty

  def test[Thunk, Output, Expected, Actual]
  (desc: String)
  (thunk: R => F[Output])
  (
    implicit
    result: TestResult[F, Output, Expected, Actual],
    sync: Sync[F],
  )
  : Unit =
    tests += ResourceTest(result, reporter)(desc)(res => TestFunction(thunk(res)))
}

trait TestInterface
{
  def reporter: TestReporter

  val tests: KlkTests =
    KlkTests()
}

abstract class Test[F[_]: TestEffect]
extends TestInterface
{
  def test
  (desc: String)
  : TestBuilder[F, HNil] =
    TestBuilder(tests, desc)(reporter)(TestResources(HNil))

  def sharedResource[R](resource: Resource[F, R])(implicit bracket: Bracket[F, Throwable]): SharedResource[F, R] = {
    val r = SharedResource(resource)(reporter)
    tests.resource(r)
    r
  }

  // import scala.language.implicitConversions

  // implicit def test0[T, A, B](t: => T)(implicit output: ConstructTest[IO, () => T, A, B]): () => T =
  //   () => output match { case _ => t }
}

object Test
{
  def execute[F[_]: Sync, T, P, O, E, A, Res]
  (result: TestResult[F, O, E, A], reporter: TestReporter)
  (desc: String)
  (thunk: Res => TestFunction[F, O])
  (log: TestLog)
  (res: Res)
  : F[FinalResult] =
    for {
      testResult <- result.handle(TestFunction.execute(thunk(res)))
      _ <- TestReporter.report[F, E, A](reporter, log)(desc)(testResult)
    } yield FinalResult()

  def forallNoShrink[F[_]: Sync, T, P, E, A, Res]
  (propGen: PropGen[F, T], result: TestResult[F, PropertyTestResult, E, A], reporter: TestReporter, effect: TestEffect[F])
  (desc: String)
  (thunk: Res => T)
  (log: TestLog)
  (res: Res)
  : F[FinalResult] =
    execute(result, reporter)(desc)((res: Res) => PropGen.noShrink(effect.concurrentPool)(thunk(res))(effect.sync, propGen))(log)(res)
}

object PlainTest
{
  def apply[F[_]: Sync, T, P, O, E, A]
  (result: TestResult[F, O, E, A], reporter: TestReporter)
  (desc: String)
  (thunk: TestFunction[F, O])
  : KlkTest[F, Unit] =
    KlkTest(desc, Test.execute(result, reporter)(desc)(_ => thunk))

  def forallNoShrink[F[_]: Sync, T, P, E, A]
  (propGen: PropGen[F, T], result: TestResult[F, PropertyTestResult, E, A], reporter: TestReporter, effect: TestEffect[F])
  (desc: String)
  (thunk: T)
  : KlkTest[F, Unit] =
    KlkTest(
      desc,
      Test.forallNoShrink(propGen, result, reporter, effect)(desc)(_ => thunk),
    )
}

object ResourceTest
{
  def execute[F[_]: Sync, T, P, O, E, A, Res]
  (result: TestResult[F, O, E, A], reporter: TestReporter)
  (desc: String)
  (thunk: Res => TestFunction[F, O])
  (log: TestLog)
  (res: Res)
  : F[FinalResult] =
    for {
      testResult <- result.handle(TestFunction.execute(thunk(res)))
      _ <- TestReporter.report[F, E, A](reporter, log)(desc)(testResult)
    } yield FinalResult()

  def apply[F[_]: Sync, T, P, O, E, A, Res]
  (result: TestResult[F, O, E, A], reporter: TestReporter)
  (desc: String)
  (thunk: Res => TestFunction[F, O])
  : KlkTest[F, Res] =
    KlkTest(desc, execute(result, reporter)(desc)(thunk))

//   def forall[F[_], T, P, O, E, A, Res]
//   (input: PropGen[F, T], result: TestResult[F, PropertyTestResult, E, A], effect: TestEffect[F])
//   (desc: String)
//   (thunk: Res => T)
//   : SharedResourceTest[F, Res, E, A] =
//     SharedResourceTest(
//       desc,
//       log => res => result.handle(TestFunction.execute(PropGen(effect.concurrentPool)(thunk(res))(effect.sync, input))),
//       effect,
//     )
}
