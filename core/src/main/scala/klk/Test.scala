package klk

import scala.collection.mutable
import scala.util.control.NonFatal

import cats.Functor
import cats.effect.{Bracket, Resource, Sync}
import cats.implicits._
import shapeless.{::, HList, HNil}

case class BasicTestResources[F[_], G[_], O](
  compile: Compile[F, G],
  result: TestResult[G, O],
  reporter: TestReporter,
)

case class KlkTests[F[_]]()
{
  val tests: mutable.Buffer[TestThunk] =
    mutable.Buffer.empty

  def add(test: TestThunk): Unit =
    tests += test

  def plain(test: KlkTest[F, Unit])(implicit compute: Compute[F], functor: Functor[F]): Unit =
    add(TestThunk(log => KlkTest.runPlain(log)(compute)(test)))

  def resource[SharedRes]
  (r: SharedResource[F, SharedRes])
  (implicit bracket: Bracket[F, Throwable])
  : Unit =
    add(TestThunk(log => KlkTest.runResource(log)(r.compute)(r.resource)(r.tests.toList)))
}

case class TestResources[ResParams <: HList](resources: ResParams)

object TestResources
{
  def empty: TestResources[HNil] =
    TestResources(HNil)
}

case class ForAllBuilder[RunF[_], ResParams <: HList, Trans]
(tests: KlkTests[RunF], desc: String)
(reporter: TestReporter)
(resources: TestResources[ResParams])
{
  def apply[TestF[_], Thunk]
  (thunk: Thunk)
  (
    implicit
    transform: TransformTestThunk[RunF, ResParams, Thunk, PropertyTestOutput[Trans]],
    compute: Compute[RunF],
    syncRun: Sync[RunF],
  )
  : Unit =
    tests.plain(KlkTest(desc, Test.execute(desc)(reporter)(_ => transform(resources)(thunk))))
}

case class TestBuilder[RunF[_], ResParams <: HList]
(tests: KlkTests[RunF], desc: String)
(reporter: TestReporter)
(resources: TestResources[ResParams])
(add: RunF[KlkResult] => Unit)
{
  def apply[TestF[_], Thunk, Output]
  (thunk: Thunk)
  (implicit transform: TransformTestThunk[RunF, ResParams, Thunk, Output])
  : Unit =
    add(transform(resources)(thunk))

  def forallNoShrink: ForAllBuilder[RunF, ResParams, PropTrans.Full] =
    ForAllBuilder(tests, desc)(reporter)(resources)

  def forall: ForAllBuilder[RunF, ResParams, PropTrans.Shrink] =
    ForAllBuilder(tests, desc)(reporter)(resources)

  def resource[TestF[_], R](r: Resource[TestF, R]): TestBuilder[RunF, Resource[TestF, R] :: ResParams] =
    TestBuilder(tests, desc)(reporter)(TestResources(r :: resources.resources))(add)
}

trait TestInterface
{
  type RunF[A]

  def reporter: TestReporter

  val tests: KlkTests[RunF] =
    KlkTests()
}

abstract class Test[RunF0[_]: Compute: Sync]
extends TestInterface
{
  type RunF[A] = RunF0[A]

  def add(desc: String)(thunk: RunF[KlkResult]): Unit =
    tests.plain(KlkTest(desc, Test.execute(desc)(reporter)(_ => thunk)))

  def test(desc: String): TestBuilder[RunF, HNil] =
    TestBuilder(tests, desc)(reporter)(TestResources[HNil](HNil))(add(desc))

  def sharedResource[R]
  (resource: Resource[RunF, R])
  : SharedResource[RunF, R] =
  {
    val r = SharedResource(resource)(reporter)
    tests.resource(r)
    r
  }
}

object Test
{
  def execute[RunF[_]: Sync, O, SharedRes]
  (desc: String)
  (reporter: TestReporter)
  (thunk: SharedRes => RunF[KlkResult])
  (log: TestLog)
  (res: SharedRes)
  : RunF[KlkResult] =
    for {
      testResult <- thunk(res)
        .recover { case NonFatal(a) => KlkResult(false, KlkResultDetails.Fatal(a)) }
      _ <- TestReporter.report[RunF](reporter, log)(desc)(testResult)
    } yield testResult
}
