package klk

import cats.data.Kleisli
import cats.effect.IO
import org.scalacheck.ForAllNoShrink
import org.scalacheck.Gen.Parameters
import org.scalacheck.Test.{Parameters => TestParameters}
import org.specs2.mutable.Specification

class ForAllTest
extends Specification
{
  "forall" >> {
    val f: PropertyTest[IO] = ForAllNoShrink { (a: Int) =>
      ForAllNoShrink { (b: Int) =>
        PropertyTest(Kleisli.pure(PropResult.bool(a != b)))
      }
    }
    val params = ScalacheckParams.cons(TestParameters.default, Parameters.default.withInitialSeed(10L))
    val result = PropertyTest.run(ConsConcurrent.io)(params)(f).unsafeRunSync()
    result.success.must(beFalse)
  }
}
