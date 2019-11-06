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
  private[this] case class Add()
  extends TestAdder[RunF, Id, λ[a => RunF[KlkResult[a]]]]
  {
    def apply[A](thunk: Id[RunF[KlkResult[A]]]): RunF[KlkResult[A]] = {
      thunk
    }
  }

  def test[A]
  (f: TestBuilder[RunF, HNil, Id, λ[a => RunF[KlkResult[a]]]] => RunF[KlkResult[Unit]])
  : KlkResult[Unit] = {
    val th: RunF[KlkResult[Unit]] = f(TestBuilder.cons(Add()))
    val kt = KlkTest.plain("test")(th)
    KlkTest.runPlain(kt)(NoopResources)
  }

  def assertWith
  (desc: String)
  (f: TestBuilder[RunF, HNil, Id, λ[a => RunF[KlkResult[a]]]] => RunF[KlkResult[Unit]])
  (check: KlkResult[Unit] => MatchResult[Any])
  : Fragment =
    desc >> test(f).must(check)

  def assert
  (desc: String)
  (f: TestBuilder[RunF, HNil, Id, λ[a => RunF[KlkResult[a]]]] => RunF[KlkResult[Unit]])
  (target: KlkResult[Unit])
  : Fragment =
    assertWith(desc)(f)(_ === target)
}

abstract class KlkSharedResourceSpecification[RunF[_]: Sync: Compute: TestFramework[*[_], NoopResources.type], R]
extends Specification
{
  def resource: Resource[RunF, R]

  private[this] case class Add()
  extends TestAdder[RunF, R => *, λ[a => R => RunF[KlkResult[a]]]]
  {
    def apply[A](thunk: R => RunF[KlkResult[A]]): R => RunF[KlkResult[A]] = {
      thunk
    }
  }

  def test
  (f: TestBuilder[RunF, HNil, R => *, λ[a => R => RunF[KlkResult[a]]]] => List[R => RunF[KlkResult[Unit]]])
  : KlkResult[Unit] = {
    val sr = SharedResource.cons[RunF, R]
    val thunks: List[R => RunF[KlkResult[Unit]]] = f(TestBuilder.cons(Add()))
    val kt = thunks.map(th => KlkTest.cons("test")(th))
    KlkTest.runResource(resource)(kt)(NoopResources)
  }

  def assert
  (desc: String)
  (f: TestBuilder[RunF, HNil, R => *, λ[a => R => RunF[KlkResult[a]]]] => List[R => RunF[KlkResult[Unit]]])
  (target: KlkResult[Unit])
  : Fragment =
    desc >> test(f).must_==(target)
}
