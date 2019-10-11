package klk

import cats.{Applicative, Monad}
import cats.data.NonEmptyList
import cats.implicits._
import org.scalacheck.Prop
import org.typelevel.discipline.Laws

final class LawsParams

sealed trait LawsResult

object LawsResult
{
  case object Empty
  extends LawsResult

  case class Executed(results: NonEmptyList[PropertyTestResult])
  extends LawsResult

  def empty: LawsResult =
    Empty

  def klkResult: LawsResult => KlkResult = {
    case Executed(results) =>
      KlkResult.Multi(results.map(PropertyTestResult.klkResult))
    case Empty =>
      KlkResult.failure(KlkResult.Details.Simple(List("no properties in law test")))
  }

  implicit def TestResult_LawsResult: TestResult[LawsResult] =
    new TestResult[LawsResult] {
      def apply(result: LawsResult): KlkResult =
        klkResult(result)
    }
}

trait FunctorialLaws[Class[_[A]], Subject[_]]

object LawsTest
{
  def rule[F[_]: Applicative](propRun: PropRun.Aux[F[Prop], PropTrans.Shrink, F])(prop: Prop): F[PropertyTestResult] =
    PropRun(propRun)(prop.pure[F])

  // TODO parent props
  def apply[F[_]: Applicative, L <: Laws]
  (propRun: PropRun.Aux[F[Prop], PropTrans.Shrink, F])
  (rules: L#RuleSet)
  : F[LawsResult] =
    rules.all.properties.toList.map(_._2) match {
      case head :: tail =>
        NonEmptyList(head, tail).traverse(rule[F](propRun)).map(LawsResult.Executed(_))
      case Nil =>
        LawsResult.empty.pure[F]
    }
}

trait LawsRun[Thunk]
{
  type TestF[A]

  def apply(thunk: Thunk): TestF[LawsResult]
}

object LawsRun
{
  type Aux[Thunk, TestF0[_]] = LawsRun[Thunk] { type TestF[A] = TestF0[A] }

  implicit def LawsRun_Any[F[_]: Monad, L <: Laws]
  (implicit propRun: PropRun.Aux[F[Prop], PropTrans.Shrink, F])
  : Aux[F[L#RuleSet], F] =
    new LawsRun[F[L#RuleSet]] {
      type TestF[A] = F[A]

      def apply(thunk: F[L#RuleSet]): F[LawsResult] =
        thunk.flatMap(LawsTest[F, L](propRun)(_))
    }

  def apply[Thunk](lawsRun: LawsRun[Thunk])(thunk: Thunk): lawsRun.TestF[LawsResult] =
    lawsRun(thunk)
}
