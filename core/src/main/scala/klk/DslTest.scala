package klk

import scala.collection.mutable

import cats.{Id, Monad}
import cats.data.Const
import cats.effect.{Bracket, Resource}
import shapeless.HNil

case class DslTests[RunF[_]: Monad: Compute: TestFramework[*[_], FR], FR]
(tests: mutable.Buffer[Suite[RunF, Unit, Unit]])
{
  def add[A](test: Suite[RunF, Unit, A]): Unit =
    tests += test.void

  def plain[A](test: KlkTest[RunF, Unit, A]): Unit =
    add(Suite.single(test).void)

  def resource[SharedRes]
  (resource: Resource[RunF, SharedRes], builder: SharedResource[RunF, SharedRes])
  (implicit bracket: Bracket[RunF, Throwable])
  : Unit =
    builder.tests.toList match {
      case head :: tail =>
        add(Suite.resource(resource, Suite.sequential(head, tail: _*)))
      case Nil =>
        add(Suite.Pure(()))
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

  def tests: Suite[RunF, Unit, Unit] =
    testsDsl.tests.toList match {
      case head :: tail =>
        Suite.sequential(head, tail: _*)
      case Nil =>
        Suite.Pure(())
    }

  private[this] case class Add(desc: String)
  extends TestAdder[RunF, Id, Const[Unit, *]]
  {
    def apply[A](thunk: Id[RunF[KlkResult[A]]]): Const[Unit, A] = {
      testsDsl.plain(KlkTest.plain(desc)(thunk))
      Const(())
    }
  }

  def test(desc: String): TestBuilder[RunF, HNil, Id, Const[Unit, *]] =
    TestBuilder(TestResources.empty)(Add(desc))

  def sharedResource[R]
  (resource: Resource[RunF, R])
  : SharedResource[RunF, R] = {
    val res = SharedResource.cons[RunF, R]
    testsDsl.resource(resource, res)
    res
  }
}
