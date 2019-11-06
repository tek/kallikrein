package klk

import cats.Show
import cats.data.NonEmptyList
import cats.kernel.Eq

trait SimpleAssertions
{
  def assert(desc: String)(value: Boolean): KlkResult[Unit] =
    KlkResult(value)(KlkResult.Details.Simple(NonEmptyList.one(desc)))

  def assertEqual[A: Show](target: A)(candidate: A)(implicit eql: Eq[A]): KlkResult[A] =
    KlkResult.Single(
      candidate,
      eql.eqv(target, candidate),
      KlkResult.Details.Complex(NonEmptyList.one("values are not equal"), target.show, candidate.show),
    )
}

trait SimpleTestBase[F[_], FR]
extends DslTest[F, FR]
with SimpleAssertions
