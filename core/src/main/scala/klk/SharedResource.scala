package klk

import scala.collection.mutable

import cats.effect.Sync
import shapeless.HNil

case class SharedResource[RunF[_]: Sync, SharedRes]
(tests: mutable.Buffer[KlkTest[RunF, SharedRes]])
(reporter: TestReporter)
{
  def add(desc: String)(thunk: SharedRes => RunF[KlkResult]): Unit =
    tests += KlkTest(desc, Test.execute(desc)(reporter)(thunk))

  def test(desc: String): TestBuilder[RunF, HNil, Function1[SharedRes, *]] =
    TestBuilder(TestResources.empty)(add(desc))
}
