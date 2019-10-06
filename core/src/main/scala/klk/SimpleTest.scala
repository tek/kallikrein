package klk

import scala.concurrent.ExecutionContext

import cats.Show
import cats.effect.{IO, Timer}
import cats.implicits._
import cats.kernel.Eq

trait SimpleAssertions
{
  def assert(desc: String)(value: Boolean): KlkResult =
    KlkResult(value, KlkResultDetails.Simple(List(desc)))

  def assertEqual[A: Show](target: A)(candidate: A)(implicit eql: Eq[A]): KlkResult =
    KlkResult(
      eql.eqv(target, candidate),
      KlkResultDetails.Complex(List("values are not equal"), target.show, candidate.show),
    )
}

trait SimpleTest[F[_]]
extends Test[F]
with SimpleAssertions
{
  def reporter: TestReporter =
    TestReporter.stdout

  implicit def timer: Timer[IO] =
    IO.timer(ExecutionContext.global)
}
