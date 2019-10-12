package klk

import cats.Id
import cats.effect.{IO, Sync}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragment
import shapeless.HNil

object TestTest
{
  def simple[A](name: String)(thunk: IO[A]): MatchResult[Any] =
    ???
}

case class TestTest(tests: Tests[SbtResources])
extends TestInterface[SbtResources]

class KlkSpecification[RunF[_]: Sync: TestFramework[*[_], NoopResources.type]]
(implicit compute: Compute[RunF])
extends Specification
{
  def test[TestShape[_]]
  (desc: String)
  (f: TestBuilder[RunF, HNil, Id, RunF[KlkResult]] => RunF[KlkResult])
  : KlkResult = {
      val th: RunF[KlkResult] = f(TestBuilder.cons(identity))
      val kt = KlkTest[RunF, Unit](desc, Test.execute(desc)(_ => th))
      KlkTest.runPlain(kt)(NoopResources)
    }

  def assert[TestShape[_]]
  (desc: String)
  (f: TestBuilder[RunF, HNil, Id, RunF[KlkResult]] => RunF[KlkResult])
  (target: KlkResult)
  : Fragment = {
    desc >> {
      test(desc)(f).must_==(target)
    }
  }
}

case class TestException()
extends Throwable

class ExceptionTest
extends KlkSpecification[IO]
{
  def frame1: Boolean =
    throw TestException()

  val target: KlkResult =
    KlkResult.Single(false, KlkResult.Details.Fatal(TestException()))

  assert[Id]("exception")(_.apply(IO(frame1)))(target)
}
