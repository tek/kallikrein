package klk

import cats.data.NonEmptyList
import cats.kernel.Monoid

sealed trait KlkResult

object KlkResult
extends KlkResultInstances
{
  sealed trait Details

  object Details
  {
    case class NoDetails()
    extends Details

    case class Simple(info: NonEmptyList[String])
    extends Details

    case class Complex(desc: NonEmptyList[String], target: String, actual: String)
    extends Details

    case class Fatal(error: Throwable)
    extends Details
  }

  case object Zero
  extends KlkResult

  case class Single(success: Boolean, details: Details)
  extends KlkResult

  case class Multi(results: NonEmptyList[KlkResult])
  extends KlkResult

  def apply(success: Boolean)(details: Details): KlkResult =
    Single(success, details)

  def success: Details => KlkResult =
    apply(true)

  def failure: Details => KlkResult =
    apply(false)

  def simpleFailure: NonEmptyList[String] => KlkResult =
    failure.compose(Details.Simple)

  def bool(success: Boolean): KlkResult =
    Single(success, Details.NoDetails())

  def successful: KlkResult => Boolean = {
    case Zero => false
    case Single(s, _) => s
    case Multi(results) => results.filterNot(successful).isEmpty
  }

  def list: KlkResult => List[KlkResult] = {
    case Zero => Nil
    case a @ Single(_, _) => List(a)
    case Multi(results) => results.toList
  }

  def combine: KlkResult => KlkResult => KlkResult = {
    case Zero => {
      case Zero => Zero
      case a @ Single(_, _) => a
      case a @ Multi(_) => a
    }
    case a @ Single(_, _) => b => Multi(NonEmptyList(a, list(b)))
    case Multi(NonEmptyList(h, t)) => b => Multi(NonEmptyList(h, t ++ list(b)))
  }

  def failures: KlkResult => List[Details] = {
    case Single(false, d) => List(d)
    case Multi(results) => results.toList.flatMap(failures)
    case _ => Nil
  }

}

private[klk] trait KlkResultInstances
{
  implicit def Monoid_KlkResult: Monoid[KlkResult] =
    new Monoid[KlkResult] {
      def empty: KlkResult = KlkResult.Zero
      def combine(x: KlkResult, y: KlkResult): KlkResult = KlkResult.combine(x)(y)
    }
}
