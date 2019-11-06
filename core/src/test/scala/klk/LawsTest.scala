package klk

import cats.Functor
import cats.effect.IO
import cats.kernel.Eq
import cats.laws.discipline.FunctorTests
import org.scalacheck.ScalacheckShapeless._
import org.specs2.matcher.MatchResult

case class Funky[A](a: A)

object Funky
{
  implicit def Functor_Funky: Functor[Funky] =
    new Functor[Funky] {
      def map[A, B](fa: Funky[A])(f: A => B): Funky[B] =
        Funky(f(fa.a))
    }

  implicit def Eq_Funky[A: Eq]: Eq[Funky[A]] =
    Eq.fromUniversalEquals
}

class FunctorLawsTest
extends KlkSpecification[IO]
{
  val target: KlkResult[Unit] =
    KlkResult.success(KlkResult.Details.NoDetails)

  val check: KlkResult[Unit] => MatchResult[Any] =
    a => KlkResult.successful(a).must_==(true)

  assertWith("laws")(_.laws(IO.pure(FunctorTests[Funky].functor[Int, Int, Int])))(check)
}
