package klk

import scala.concurrent.ExecutionContext

import cats.effect.{IO, Timer}
import cats.kernel.Eq

trait SimpleAssertions
{
  def assert(desc: String)(value: Boolean): KlkResult[Boolean, Boolean] =
    KlkResult(value, KlkResultDetails.Simple(List(desc)))

  def assertEqual[A](target: A)(candidate: A)(implicit eql: Eq[A]): KlkResult[A, A] =
    KlkResult(eql.eqv(target, candidate), KlkResultDetails.Complex(List("values are not equal"), target, candidate))
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
