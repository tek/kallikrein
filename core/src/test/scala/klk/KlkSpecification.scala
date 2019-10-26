package klk

import cats.Id
import cats.effect.{Resource, Sync}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragment
import shapeless.HNil

abstract class KlkSpecification[RunF[_]: Sync: Compute: TestFramework[*[_], NoopResources.type]]
extends Specification
{
  def test
  (f: TestBuilder[RunF, HNil, Id, RunF[KlkResult]] => RunF[KlkResult])
  : KlkResult = {
    val th: RunF[KlkResult] = f(TestBuilder.cons(identity))
    val kt = KlkTest.plain("test")(th)
    KlkTest.runPlain(kt)(NoopResources)
  }

  def assertWith
  (desc: String)
  (f: TestBuilder[RunF, HNil, Id, RunF[KlkResult]] => RunF[KlkResult])
  (check: KlkResult => MatchResult[Any])
  : Fragment =
    desc >> test(f).must(check)

  def assert
  (desc: String)
  (f: TestBuilder[RunF, HNil, Id, RunF[KlkResult]] => RunF[KlkResult])
  (target: KlkResult)
  : Fragment =
    assertWith(desc)(f)(_ === target)
}

abstract class KlkSharedResourceSpecification[RunF[_]: Sync: Compute: TestFramework[*[_], NoopResources.type], R]
extends Specification
{
  def resource: Resource[RunF, R]

  def test
  (f: TestBuilder[RunF, HNil, Function1[R, *], R => RunF[KlkResult]] => List[R => RunF[KlkResult]])
  : KlkResult = {
    val sr = SharedResource.cons[RunF, R]
    val thunks: List[R => RunF[KlkResult]] = f(TestBuilder.cons(identity))
    val kt = thunks.map(th => KlkTest.cons("test")(th))
    KlkTest.runResource(resource)(kt)(NoopResources)
  }

  def assert
  (desc: String)
  (f: TestBuilder[RunF, HNil, Function1[R, *], R => RunF[KlkResult]] => List[R => RunF[KlkResult]])
  (target: KlkResult)
  : Fragment =
    desc >> test(f).must_==(target)
}
