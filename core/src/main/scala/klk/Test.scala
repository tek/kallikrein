package klk

import scala.collection.mutable
import scala.util.control.NonFatal

import cats.Functor
import cats.effect.{Bracket, Resource, Sync}
import cats.implicits._
import shapeless.{::, HList, HNil}

case class KlkTests[F[_]]()
{
  val tests: mutable.Buffer[TestThunk] =
    mutable.Buffer.empty

  def add(test: TestThunk): Unit =
    tests += test

  def plain(test: KlkTest[F, Unit])(implicit compute: Compute[F], functor: Functor[F]): Unit =
    add(TestThunk(log => KlkTest.runPlain(log)(compute)(test)))

  def resource[SharedRes]
  (resource: Resource[F, SharedRes], tests: mutable.Buffer[KlkTest[F, SharedRes]])
  (implicit bracket: Bracket[F, Throwable], compute: Compute[F])
  : Unit =
    add(TestThunk(log => KlkTest.runResource(log)(compute)(resource)(tests.toList)))
}

case class TestResources[ResParams <: HList](resources: ResParams)

object TestResources
{
  def empty: TestResources[HNil] =
    TestResources(HNil)
}

case class ForAllBuilder[RunF[_], ResParams <: HList, Trans]
(resources: TestResources[ResParams])
(add: RunF[KlkResult] => Unit)
{
  def apply[Thunk]
  (thunk: Thunk)
  (implicit transform: TransformTestThunk[RunF, ResParams, Thunk, PropertyTestOutput[Trans]])
  : Unit =
    add(transform(resources)(thunk))
}

case class TestBuilder[RunF[_], ResParams <: HList]
(resources: TestResources[ResParams])
(add: RunF[KlkResult] => Unit)
{
  def apply[TestF[_], Thunk, Output]
  (thunk: Thunk)
  (implicit transform: TransformTestThunk[RunF, ResParams, Thunk, Output])
  : Unit =
    add(transform(resources)(thunk))

  def forallNoShrink: ForAllBuilder[RunF, ResParams, PropTrans.Full] =
    ForAllBuilder(resources)(add)

  def forall: ForAllBuilder[RunF, ResParams, PropTrans.Shrink] =
    ForAllBuilder(resources)(add)

  def resource[TestF[_], R](r: Resource[TestF, R]): TestBuilder[RunF, Resource[TestF, R] :: ResParams] =
    TestBuilder(TestResources(r :: resources.resources))(add)
}

trait TestInterface
{
  type RunF[A]

  private[klk] def reporter: TestReporter

  private[klk] val tests: KlkTests[RunF] =
    KlkTests()
}

abstract class Test[RunF0[_]: Compute: Sync]
extends TestInterface
{
  type RunF[A] = RunF0[A]

  private[this] def add(desc: String)(thunk: RunF[KlkResult]): Unit =
    tests.plain(KlkTest(desc, Test.execute(desc)(reporter)(_ => thunk)))

  def test(desc: String): TestBuilder[RunF, HNil] =
    TestBuilder(TestResources.empty)(add(desc))

  def sharedResource[R](resource: Resource[RunF, R]): SharedResource[RunF, R] = {
    val rTests: mutable.Buffer[KlkTest[RunF, R]] = mutable.Buffer.empty
    tests.resource(resource, rTests)
    SharedResource[RunF, R](rTests)(reporter)
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
