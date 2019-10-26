package klk

import scala.collection.mutable

import cats.{Id, Monad}
import cats.effect.{Bracket, Resource}
import shapeless.HNil

case class DslTests[RunF[_]: Monad: Compute: TestFramework[*[_], FR], FR](tests: mutable.Buffer[TestAlg[RunF, Unit, Unit]])
{
  def add(test: TestAlg[RunF, Unit, Unit]): Unit =
    tests += test

  def plain(test: KlkTest[RunF, Unit]): Unit =
    add(TestAlg.single(test))

  def resource[SharedRes]
  (resource: Resource[RunF, SharedRes], builder: SharedResource[RunF, SharedRes])
  (implicit bracket: Bracket[RunF, Throwable])
  : Unit =
    builder.tests.toList match {
      case head :: tail =>
        add(TestAlg.resource(resource, TestAlg.sequential(head, tail: _*)))
      case Nil =>
        add(TestAlg.Pure(TestAlg.Output.Zero))
    }
}

object DslTests
{
  def cons[RunF[_]: Monad: Compute: TestFramework[*[_], FR], FR]: DslTests[RunF, FR] =
    DslTests(mutable.Buffer.empty)
}

abstract class DslTest[RunF[_]: Monad: Compute: Bracket[*[_], Throwable]: MeasureTest: TestFramework[*[_], FR], FR]
extends TestBase[RunF, FR]
{
  private[this] val testsDsl: DslTests[RunF, FR] =
    DslTests.cons

  def tests: TestAlg[RunF, Unit, Unit] =
    testsDsl.tests.toList match {
      case head :: tail =>
        TestAlg.sequential(head, tail: _*)
      case Nil =>
        TestAlg.Pure(TestAlg.Output.Zero)
    }

  private[this] def add
  (desc: String)
  (thunk: Id[RunF[KlkResult]])
  : Unit =
    testsDsl.plain(KlkTest.plain(desc)(thunk))

  def test(desc: String): TestBuilder[RunF, HNil, Id, Unit] =
    TestBuilder(TestResources.empty)(add(desc))

  def sharedResource[R]
  (resource: Resource[RunF, R])
  : SharedResource[RunF, R] = {
    val res = SharedResource.cons[RunF, R]
    testsDsl.resource(resource, res)
    res
  }
}
