package klk

import cats.data.NonEmptyList
import cats.kernel.Monoid

sealed trait KlkResultDetails

object KlkResultDetails
{
  case class NoDetails()
  extends KlkResultDetails

  case class Simple(info: List[String])
  extends KlkResultDetails

  case class Complex(desc: List[String], target: String, actual: String)
  extends KlkResultDetails

  case class Fatal(error: Throwable)
  extends KlkResultDetails
}

sealed trait KlkResult

object KlkResult
extends KlkResultInstances
{
  case object Zero
  extends KlkResult

  case class Single(success: Boolean, details: KlkResultDetails)
  extends KlkResult

  case class Multi(results: NonEmptyList[KlkResult])
  extends KlkResult

  def apply(success: Boolean, details: KlkResultDetails): KlkResult =
    Single(success, details)

  def bool(success: Boolean): KlkResult =
    Single(success, KlkResultDetails.NoDetails())

  def success: KlkResult => Boolean = {
    case Zero => false
    case Single(s, _) => s
    case Multi(results) => results.exists(success)
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

  def failures: KlkResult => List[KlkResultDetails] = {
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
