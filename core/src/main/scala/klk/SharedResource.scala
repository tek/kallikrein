package klk

import scala.collection.mutable

import cats.effect.Sync
import shapeless.HNil

case class SharedResource[RunF[_]: Sync, SharedRes]
(tests: mutable.Buffer[KlkTest[RunF, SharedRes]])
{
  def add(desc: String)(thunk: SharedRes => RunF[KlkResult]): Unit =
    tests += KlkTest(desc, Test.execute(desc)(thunk))

  def test(desc: String): TestBuilder[RunF, HNil, Function1[SharedRes, *], Unit] =
    TestBuilder(TestResources.empty)(add(desc))
}

object SharedResource
{
  def cons[RunF[_]: Sync, SharedRes]: SharedResource[RunF, SharedRes] =
    SharedResource(mutable.Buffer.empty)
}
