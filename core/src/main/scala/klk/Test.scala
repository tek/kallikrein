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

case class TestResources[ResParams <: HList](resources: ResParams)

case class TestBuilder[TestF[_], RunF[_], ResParams <: HList]
(tests: KlkTests, desc: String)
(reporter: TestReporter)
(resources: TestResources[ResParams])
{
  def apply[Thunk, Thunk0, Output]
  (thunk: Thunk)
  (
    implicit
    strip: StripResources.Aux[RunF, ResParams, Thunk, TestF[Output]],
    compile: Compile[TestF, RunF],
    result: TestResult[RunF, Output],
    effect: Compute[RunF],
    syncRun: Sync[RunF],
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
    strip: StripResources.Aux[RunF, ResParams, Thunk, Thunk0],
    propGen: PropGen[TestF, Thunk0, Trans],
    compile: Compile[TestF, RunF],
    result: TestResult[RunF, PropertyTestResult],
    effect: Compute[RunF],
    pool: ConsConcurrent[TestF],
    syncTest: Sync[TestF],
    syncRun: Sync[RunF],
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
    strip: StripResources.Aux[RunF, ResParams, Thunk, Thunk0],
    propGen: PropGen[TestF, Thunk0, PropRunner.Full],
    compile: Compile[TestF, RunF],
    result: TestResult[RunF, PropertyTestResult],
    effect: Compute[RunF],
    pool: ConsConcurrent[TestF],
    syncF: Sync[TestF],
    syncG: Sync[RunF],
  )
  : Unit =
    genForall(thunk)

  def forall[Thunk, Thunk0]
  (thunk: Thunk)
  (
    implicit
    strip: StripResources.Aux[RunF, ResParams, Thunk, Thunk0],
    propGen: PropGen[TestF, Thunk0, PropRunner.Shrink],
    compile: Compile[TestF, RunF],
    result: TestResult[RunF, PropertyTestResult],
    effect: Compute[RunF],
    pool: ConsConcurrent[TestF],
    syncF: Sync[TestF],
    syncG: Sync[RunF],
  )
  : Unit =
    genForall(thunk)

  def resource[R](r: Resource[TestF, R]): TestBuilder[TestF, RunF, Resource[TestF, R] :: ResParams] =
    TestBuilder(tests, desc)(reporter)(TestResources(r :: resources.resources))
}

case class SharedResourceTestBuilder[RunF[_], TestF[_], R]
(desc: String)
(tests: mutable.Buffer[KlkTest[RunF, R]])
(reporter: TestReporter)
{
  def apply[Thunk, Output]
  (thunk: R => TestF[Output])
  (
    implicit
    compile: Compile[TestF, RunF],
    result: TestResult[RunF, Output],
    syncG: Sync[RunF],
  )
  : Unit =
    tests += SharedResourceTest(BasicTestResources(compile, result, reporter))(desc)(r => Resource.pure(thunk(r)))
}

case class SharedResource[RunF[_], R]
(resource: Resource[RunF, R])
(reporter: TestReporter)
(implicit val effect: Compute[RunF])
{
  val tests: mutable.Buffer[KlkTest[RunF, R]] =
    mutable.Buffer.empty

  def test[TestF[_]](desc: String): SharedResourceTestBuilder[RunF, TestF, R] =
    SharedResourceTestBuilder(desc)(tests)(reporter)
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
  def execute[TestF[_], RunF[_]: Sync, O, SharedRes]
  (resources: BasicTestResources[TestF, RunF, O])
  (desc: String)
  (thunk: SharedRes => Resource[RunF, TestF[O]])
  (log: TestLog)
  (res: SharedRes)
  : RunF[KlkResult] =
    for {
      testResult <- resources.result.handle(thunk(res).use(resources.compile(_)))
        .recover { case NonFatal(a) => KlkResult(false, KlkResultDetails.Fatal(a)) }
      _ <- TestReporter.report[RunF](resources.reporter, log)(desc)(testResult)
    } yield testResult

  def forallThunk[TestF[_]: Sync, RunF[_]: Sync, SharedRes, T, Tr]
  (propGen: PropGen[TestF, T, Tr], concurrent: ConsConcurrent[TestF])
  (thunk: SharedRes => Resource[RunF, T])
  (res: SharedRes)
  : Resource[RunF, TestF[PropertyTestResult]] =
    thunk(res).map(t => PropGen(concurrent.pool)(t)(propGen))

  def forall[TestF[_]: Sync, RunF[_]: Sync, T, Tr, P, O, SharedRes]
  (resources: PropTestResources[TestF, RunF, T, Tr])
  (desc: String)
  (thunk: SharedRes => Resource[RunF, T])
  (log: TestLog)
  (res: SharedRes)
  : RunF[KlkResult] =
    execute(resources.basic)(desc)(forallThunk(resources.propGen, resources.pool)(thunk))(log)(res)
}

object PlainTest
{
  def apply[TestF[_], RunF[_]: Sync, P, O]
  (resources: BasicTestResources[TestF, RunF, O])
  (desc: String)
  (thunk: Resource[RunF, TestF[O]])
  : KlkTest[RunF, Unit] =
    KlkTest(desc, Test.execute(resources)(desc)(_ => thunk))

  def forall[TestF[_]: Sync, RunF[_]: Sync, T, Tr, P]
  (resources: PropTestResources[TestF, RunF, T, Tr])
  (desc: String)
  (thunk: Resource[RunF, T])
  : KlkTest[RunF, Unit] =
    KlkTest(desc, Test.forall(resources)(desc)(_ => thunk))
}

object SharedResourceTest
{
  def apply[TestF[_], RunF[_]: Sync, T, P, O, Res]
  (resources: BasicTestResources[TestF, RunF, O])
  (desc: String)
  (thunk: Res => Resource[RunF, TestF[O]])
  : KlkTest[RunF, Res] =
    KlkTest(desc, Test.execute(resources)(desc)(thunk))

  def forall[TestF[_]: Sync, RunF[_]: Sync, T, Tr, P, O, Res]
  (resources: PropTestResources[TestF, RunF, T, Tr])
  (desc: String)
  (thunk: Res => Resource[RunF, T])
  : KlkTest[RunF, Res] =
    KlkTest(desc, Test.forall(resources)(desc)(thunk))
}
