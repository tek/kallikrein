package klk

import scala.collection.mutable
import scala.util.control.NonFatal

import cats.{Functor, Id}
import cats.effect.{Bracket, Resource, Sync}
import cats.implicits._
import shapeless.HNil

case class KlkTests[F[_], FR](tests: mutable.Buffer[TestThunk[FR]])
{
  def add(test: TestThunk[FR]): Unit =
    tests += test

  def plain
  (test: KlkTest[F, Unit])
  (implicit compute: Compute[F], functor: Functor[F], fw: TestFramework[F, FR])
  : Unit =
    add(TestThunk(test.desc, KlkTest.runPlain(compute)(test)))

  def resource[SharedRes]
  (resource: Resource[F, SharedRes], builder: SharedResource[F, SharedRes])
  (implicit bracket: Bracket[F, Throwable], compute: Compute[F], fw: TestFramework[F, FR])
  : Unit =
    add(TestThunk("shared resource", KlkTest.runResource(compute)(resource)(builder.tests)))
}

object KlkTests
{
  def cons[F[_], FR]: KlkTests[F, FR] =
    KlkTests(mutable.Buffer.empty)
}

case class Tests[FR](tests: List[TestThunk[FR]])

private[klk] trait TestMarker

abstract class TestInterface[FR]
extends TestMarker
{
  def tests: Tests[FR]
}

abstract class Test[RunF[_]: Compute: Sync, FR]
(implicit testFramework: TestFramework[RunF, FR])
extends TestInterface[FR]
{
  private[this] val testsDsl: KlkTests[RunF, FR] =
    KlkTests.cons

  def tests: Tests[FR] =
    Tests(testsDsl.tests.toList)

  private[this] def add(desc: String)(thunk: Id[RunF[KlkResult]]): Unit =
    testsDsl.plain(KlkTest[RunF, Unit](desc, Test.execute(desc)(_ => thunk)))

  def test(desc: String): TestBuilder[RunF, HNil, Id, Unit] =
    TestBuilder(TestResources.empty)(add(desc))

  def sharedResource[R](resource: Resource[RunF, R]): SharedResource[RunF, R] = {
    val res = SharedResource.cons[RunF, R]
    testsDsl.resource(resource, res)
    res
  }
}

object Test
{
  def execute[RunF[_]: Sync, O, SharedRes, FR]
  (desc: String)
  (thunk: SharedRes => RunF[KlkResult])
  (reporter: TestReporter[RunF])
  (sharedRes: SharedRes)
  : RunF[KlkResult] =
    for {
      testResult <- thunk(sharedRes)
        .recover { case NonFatal(a) => KlkResult.failure(KlkResult.Details.Fatal(a)) }
        _ <- TestReporter.report[RunF](reporter)(desc)(testResult)
    } yield testResult
}
