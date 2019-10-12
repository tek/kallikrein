package klk

import cats.{Applicative, Functor}
import cats.data.{Kleisli, NonEmptyList}
import cats.implicits._
import org.scalacheck.Prop
import org.typelevel.discipline.Laws

final class LawsParams

object LawsParams
{
  implicit def PropThunk_Output[F0[_]: Functor]
  : PropThunk.Aux[F0[Prop], LawsParams, F0] =
    new PropThunk[F0[Prop], LawsParams] {
      type F[A] = F0[A]
      def apply(f: F[Prop]): PropertyTest[F] =
        PropertyTest(Kleisli(params => f.map(out => out(params))))
    }
}

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
  def rule[F[_]: Applicative](propRun: PropRun.Aux[F[Prop], LawsParams, F])(prop: Prop): F[PropertyTestResult] =
    PropRun(propRun)(prop.pure[F])

  def apply[F[_]: Applicative, L <: Laws]
  (propRun: PropRun.Aux[F[Prop], LawsParams, F])
  (rules: L#RuleSet)
  : F[LawsResult] =
    rules.all.properties.toList.map(_._2) match {
      case head :: tail =>
        NonEmptyList(head, tail).traverse(rule[F](propRun)).map(LawsResult.Executed(_))
      case Nil =>
        LawsResult.empty.pure[F]
    }
}

// object LawsTest
// {
//   // def rule[F[_]: Applicative](propRun: PropRun.Aux[F[Prop], PropTrans.Shrink, F])(prop: Prop): F[PropertyTestResult] =
//   //   PropRun(propRun)(prop.pure[F])

//   def propResult(result: Prop.Result): PropertyTestResult =
//     PropertyTestResult(
//       PropertyTestResult.success(result.status),
//       PropertyTestState.Stats(true, 0, 0),
//       PropTest.Result(PropTest.Status)
//     )

//   def apply[F[_]: Sync, L <: Laws]
//   (rules: L#RuleSet)
//   : F[LawsResult] =
//     rules.all.properties.toList.map(_._2) match {
//       case head :: tail =>
//         NonEmptyList(head, tail)
//           .traverse(a => Sync[F].delay(a(Gen.Parameters.default)))
//           .map(_.map(propResult))
//           .map(LawsResult.Executed(_))
//       case Nil =>
//         LawsResult.empty.pure[F]
//     }
// }
