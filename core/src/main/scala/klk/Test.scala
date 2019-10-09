package klk

import scala.collection.mutable
import scala.util.control.NonFatal

import cats.{Functor, Id}
import cats.effect.{Bracket, Resource, Sync}
import cats.implicits._
import shapeless.HNil

case class KlkTests[F[_]]()
{
  val tests: mutable.Buffer[TestThunk] =
    mutable.Buffer.empty

  def add(test: TestThunk): Unit =
    tests += test

  def plain(test: KlkTest[F, Unit])(implicit compute: Compute[F], functor: Functor[F]): Unit =
    add(TestThunk(test.desc, log => KlkTest.runPlain(log)(compute)(test)))

  def resource[SharedRes]
  (resource: Resource[F, SharedRes], tests: mutable.Buffer[KlkTest[F, SharedRes]])
  (implicit bracket: Bracket[F, Throwable], compute: Compute[F])
  : Unit =
    add(TestThunk("shared resource", log => KlkTest.runResource(log)(compute)(resource)(tests.toList)))
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

  private[this] def add(desc: String)(thunk: Id[RunF[KlkResult]]): Unit =
    tests.plain(KlkTest(desc, Test.execute(desc)(reporter)(_ => thunk)))

  def test(desc: String): TestBuilder[RunF, HNil, Id] =
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
        .recover { case NonFatal(a) => KlkResult.failure(KlkResult.Details.Fatal(a)) }
      _ <- TestReporter.report[RunF](reporter, log)(desc)(testResult)
    } yield testResult
}
