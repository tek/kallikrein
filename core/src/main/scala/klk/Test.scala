package klk

import scala.collection.mutable
import scala.util.control.NonFatal

import cats.Functor
import cats.effect.{Bracket, Resource, Sync}
import cats.implicits._
import shapeless.{::, HList, HNil}

case class KlkTests()
{
  val tests: mutable.Buffer[TestThunk] =
    mutable.Buffer.empty

  def add(test: TestThunk): Unit =
    tests += test

  def plain[F[_]: Functor](test: KlkTest[F, Unit])(implicit effect: Compute[F]): Unit =
    add(TestThunk(log => KlkTest.runPlain(log)(effect)(test)))

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

  implicit def StripResources_HNil[F[_], Output]
  : StripResources.Aux[HNil, F[Output], F[Output]] =
    new StripResources[HNil, F[Output]] {
      type Thunk = F[Output]
      def apply(resources: TestResources[HNil])(thunk: Thunk): Thunk =
        thunk
    }

  implicit def StripResources_HList[F[_]: Bracket[?[_], Throwable], H, T <: HList, ThunkF, Output]
  (implicit next: StripResources.Aux[T, ThunkF, F[Output]])
  : Aux[Resource[F, H] :: T, H => ThunkF, F[Output]] =
    new StripResources[Resource[F, H] :: T, H => ThunkF] {
      type Thunk = F[Output]
      def apply(resources: TestResources[Resource[F, H] :: T])(thunk: H => ThunkF): F[Output] =
        resources.resources.head.use(h => next(TestResources(resources.resources.tail))(thunk(h)))
    }
}

case class TestResources[R <: HList](resources: R)

case class TestBuilder[F[_], G[_], RR <: HList]
(tests: KlkTests, desc: String)
(reporter: TestReporter)
(resources: TestResources[RR])
{
  def apply[Thunk, Thunk0, Output]
  (thunk: Thunk)
  (
    implicit
    strip: StripResources.Aux[RR, Thunk, F[Output]],
    compile: Compile[F, G],
    result: TestResult[G, Output],
    effect: Compute[G],
    syncF: Sync[F],
    syncG: Sync[G],
  )
  : Unit =
  {
    val btRes = BasicTestResources(compile, result, reporter)
    tests.plain(PlainTest(btRes)(desc)(strip(resources)(thunk)))
  }

  def genForall[Thunk, Thunk0, Trans]
  (thunk: Thunk)
  (
    implicit
    strip: StripResources.Aux[RR, Thunk, Thunk0],
    propGen: PropGen[F, Thunk0, Trans],
    compile: Compile[F, G],
    result: TestResult[G, PropertyTestResult],
    effect: Compute[G],
    pool: ConsConcurrent[F],
    syncF: Sync[F],
    syncG: Sync[G],
  )
  : Unit =
  {
    val ptRes = PropTestResources(BasicTestResources(compile, result, reporter), propGen, pool)
    tests.plain(PlainTest.forall(ptRes)(desc)(strip(resources)(thunk)))
  }

  def forallNoShrink[Thunk, Thunk0]
  (thunk: Thunk)
  (
    implicit
    strip: StripResources.Aux[RR, Thunk, Thunk0],
    propGen: PropGen[F, Thunk0, PropRunner.Full],
    compile: Compile[F, G],
    result: TestResult[G, PropertyTestResult],
    effect: Compute[G],
    pool: ConsConcurrent[F],
    syncF: Sync[F],
    syncG: Sync[G],
  )
  : Unit =
    genForall(thunk)

  def forall[Thunk, Thunk0]
  (thunk: Thunk)
  (
    implicit
    strip: StripResources.Aux[RR, Thunk, Thunk0],
    propGen: PropGen[F, Thunk0, PropRunner.Shrink],
    compile: Compile[F, G],
    result: TestResult[G, PropertyTestResult],
    effect: Compute[G],
    pool: ConsConcurrent[F],
    syncF: Sync[F],
    syncG: Sync[G],
  )
  : Unit =
    genForall(thunk)

  def resource[R](r: Resource[F, R]): TestBuilder[F, G, Resource[F, R] :: RR] =
    TestBuilder(tests, desc)(reporter)(TestResources(r :: resources.resources))
}

case class SharedResource[G[_], R]
(resource: Resource[G, R])
(reporter: TestReporter)
(implicit val effect: Compute[G])
{
  val tests: mutable.Buffer[KlkTest[G, R]] =
    mutable.Buffer.empty

  def test[F[_], Thunk, Output]
  (desc: String)
  (thunk: R => F[Output])
  (
    implicit
    compile: Compile[F, G],
    result: TestResult[G, Output],
    syncF: Sync[F],
    syncG: Sync[G],
  )
  : Unit =
    tests += ResourceTest(BasicTestResources(compile, result, reporter))(desc)(thunk)
}

trait TestInterface
{
  def reporter: TestReporter

  val tests: KlkTests =
    KlkTests()
}

abstract class Test[F[_]: Compute]
extends TestInterface
{
  def test[G[_]]
  (desc: String)
  : TestBuilder[F, G, HNil] =
    TestBuilder(tests, desc)(reporter)(TestResources(HNil))

  def sharedResource[R](resource: Resource[F, R])(implicit bracket: Bracket[F, Throwable]): SharedResource[F, R] = {
    val r = SharedResource(resource)(reporter)
    tests.resource(r)
    r
  }
}

case class BasicTestResources[F[_], G[_], O](
  compile: Compile[F, G],
  result: TestResult[G, O],
  reporter: TestReporter,
)

case class PropTestResources[F[_], G[_], T, Tr](
  basic: BasicTestResources[F, G, PropertyTestResult],
  propGen: PropGen[F, T, Tr],
  pool: ConsConcurrent[F],
)

object Test
{
  def execute[F[_]: Sync, G[_]: Sync, T, P, O, Res]
  (resources: BasicTestResources[F, G, O])
  (desc: String)
  (thunk: Res => F[O])
  (log: TestLog)
  (res: Res)
  : G[KlkResult] =
    for {
      testResult <- resources.result.handle(resources.compile(thunk(res)))
        .recover { case NonFatal(a) => KlkResult(false, KlkResultDetails.Fatal(a)) }
      _ <- TestReporter.report[G](resources.reporter, log)(desc)(testResult)
    } yield testResult

  def forallThunk[F[_]: Sync, Res, T, Tr](propGen: PropGen[F, T, Tr], concurrent: ConsConcurrent[F])
  : (Res => T) => Res => F[PropertyTestResult] =
    thunk => res => PropGen(concurrent.pool)(thunk(res))(propGen)

  def forall[F[_]: Sync, G[_]: Sync, T, Tr, P, O, Res]
  (resources: PropTestResources[F, G, T, Tr])
  (desc: String)
  (thunk: Res => T)
  (log: TestLog)
  (res: Res)
  : G[KlkResult] =
    execute(resources.basic)(desc)(forallThunk(resources.propGen, resources.pool).apply(thunk))(log)(res)
}

object PlainTest
{
  def apply[F[_]: Sync, G[_]: Sync, T, P, O]
  (resources: BasicTestResources[F, G, O])
  (desc: String)
  (thunk: F[O])
  : KlkTest[G, Unit] =
    KlkTest(desc, Test.execute(resources)(desc)(_ => thunk))

  def forall[F[_]: Sync, G[_]: Sync, T, Tr, P]
  (resources: PropTestResources[F, G, T, Tr])
  (desc: String)
  (thunk: T)
  : KlkTest[G, Unit] =
    KlkTest(desc, Test.forall(resources)(desc)(_ => thunk))
}

object ResourceTest
{
  def apply[F[_]: Sync, G[_]: Sync, T, P, O, Res]
  (resources: BasicTestResources[F, G, O])
  (desc: String)
  (thunk: Res => F[O])
  : KlkTest[G, Res] =
    KlkTest(desc, Test.execute(resources)(desc)(thunk))

  def forall[F[_]: Sync, G[_]: Sync, T, Tr, P, O, Res]
  (resources: PropTestResources[F, G, T, Tr])
  (desc: String)
  (thunk: Res => T)
  : KlkTest[G, Res] =
    KlkTest(desc, Test.forall(resources)(desc)(thunk))
}
