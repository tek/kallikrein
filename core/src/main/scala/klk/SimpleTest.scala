package klk

import cats.Show
import cats.kernel.Eq

trait SimpleAssertions
{
  def assert(desc: String)(value: Boolean): KlkResult =
    KlkResult(value)(KlkResult.Details.Simple(List(desc)))

  def assertEqual[A: Show](target: A)(candidate: A)(implicit eql: Eq[A]): KlkResult =
    KlkResult(eql.eqv(target, candidate))(
      KlkResult.Details.Complex(List("values are not equal"), target.show, candidate.show),
    )
}

trait SimpleTestBase[F[_], FR]
extends DslTest[F, FR]
with SimpleAssertions
