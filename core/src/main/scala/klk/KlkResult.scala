package klk

import cats.{Functor, MonoidK}
import cats.data.NonEmptyList
import cats.kernel.Monoid

sealed trait KlkResult[+A]

object KlkResult
extends KlkResultInstances
{
  sealed trait Details

  object Details
  {
    case object NoDetails
    extends Details

    case class Simple(info: NonEmptyList[String])
    extends Details

    case class Complex(desc: NonEmptyList[String], target: String, actual: String)
    extends Details
  }

  case object Zero
  extends KlkResult[Nothing]

  case class Pure[A](a: A)
  extends KlkResult[A]

  case class Fatal(error: Throwable)
  extends KlkResult[Nothing]

  case class Single[A](value: A, success: Boolean, details: Details)
  extends KlkResult[A]

  case class Multi[A](results: NonEmptyList[KlkResult[A]])
  extends KlkResult[A]

  def apply(success: Boolean)(details: Details): KlkResult[Unit] =
    Single((), success, details)

  object Value
  {
    def unapply[A](result: KlkResult[A]): Option[A] =
      result match {
        case Pure(a) => Some(a)
        case Single(a, _, _) => Some(a)
        case Multi(results) => results.collectFirst { case Value(a) => a }
        case _ => None
      }
  }

  def success: Details => KlkResult[Unit] =
    apply(true)

  def failure: Details => KlkResult[Unit] =
    apply(false)

  def simpleFailure: NonEmptyList[String] => KlkResult[Unit] =
    failure.compose(Details.Simple)

  def valueFailure[A](value: A)(message: NonEmptyList[String]): KlkResult[A] =
    Single(value, false, Details.Simple(message))

  def bool(success: Boolean): KlkResult[Unit] =
    Single((), success, Details.NoDetails)

  def successful[A]: KlkResult[A] => Boolean = {
    case Zero => false
    case Single(_, s, _) => s
    case Multi(results) => results.filterNot(successful).isEmpty
    case Fatal(_) => false
    case Pure(_) => false
  }

  def list[A]: KlkResult[A] => List[KlkResult[A]] = {
    case Zero => Nil
    case Multi(results) => results.toList
    case a => List(a)
  }

  def combine[A]: KlkResult[A] => KlkResult[A] => KlkResult[A] = {
    case Zero => {
      case Zero => Zero
      case a @ Multi(_) => a
      case a => a
    }
    case Multi(NonEmptyList(h, t)) => b => Multi(NonEmptyList(h, t ++ list(b)))
    case a => b => Multi(NonEmptyList(a, list(b)))
  }

  def failures[A]: KlkResult[A] => List[Details] = {
    case Single(_, false, d) => List(d)
    case Multi(results) => results.toList.flatMap(failures)
    case _ => Nil
  }

  def details[A]: KlkResult[A] => List[Details] = {
    case Single(_, _, dt) => List(dt)
    case Multi(results) => results.toList >>= details[A]
    case _ => Nil
  }
}

private[klk] trait KlkResultInstances
{
  implicit def Monoid_KlkResult[A]: Monoid[KlkResult[A]] =
    new Monoid[KlkResult[A]] {
      def empty: KlkResult[A] = KlkResult.Zero
      def combine(x: KlkResult[A], y: KlkResult[A]): KlkResult[A] = KlkResult.combine(x)(y)
    }

  implicit def MonoidK_KlkResult: MonoidK[KlkResult] =
    new MonoidK[KlkResult] {
      def empty[A]: KlkResult[A] = KlkResult.Zero
      def combineK[A](x: KlkResult[A], y: KlkResult[A]): KlkResult[A] = KlkResult.combine(x)(y)
    }

  implicit def Functor_KlkResult: Functor[KlkResult] =
    new Functor[KlkResult] {
      def map[A, B](fa: KlkResult[A])(f: A => B): KlkResult[B] =
        fa match {
          case KlkResult.Single(a, success, details) =>
            KlkResult.Single(f(a), success, details)
          case KlkResult.Multi(results) =>
            KlkResult.Multi(results.map(_.map(f)))
          case KlkResult.Zero =>
            KlkResult.Zero
          case KlkResult.Fatal(error) =>
            KlkResult.Fatal(error)
          case KlkResult.Pure(a) =>
            KlkResult.Pure(f(a))
        }
    }
}
