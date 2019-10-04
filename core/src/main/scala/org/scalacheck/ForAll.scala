package org.scalacheck

import cats.data.Kleisli
import cats.effect.Sync
import cats.implicits._
import fs2.{Pull, Stream}
import klk.PropertyTest
import org.scalacheck.Gen.Parameters
import org.scalacheck.util.Pretty

object ForAll
{
  def provedToTrue(r: Prop.Result) = r.status match {
    case Prop.Proof => r.copy(status = Prop.True)
    case _ => r
  }

  def addArg[A]
  (labels: String)
  (value: A, orig: A, shrinks: Int)
  (result: Prop.Result)
  (implicit pretty: A => Pretty)
  : Prop.Result =
    result.addArg(Prop.Arg(labels, value, shrinks, orig, pretty(value), pretty(orig)))
}

object ForAllNoShrink
{
  def executeForArg[F[_]: Sync, A, P]
  (test: A => PropertyTest[F], prms: Parameters, genResult: Gen.R[A])
  (value: A)
  (implicit pp1: A => Pretty)
  : Kleisli[F, Parameters, Prop.Result] =
    Kleisli.local((_: Parameters) => prms)(test(value).test)
      .recoverWith { case e: Throwable => Kleisli.pure(Prop.Result(status = Prop.Exception(e))) }
      .map(ForAll.provedToTrue)
      .map(ForAll.addArg(genResult.labels.mkString(","))(value, value, 0))

  def apply[F[_]: Sync, A, P, Output]
  (test: A => PropertyTest[F])
  (implicit arbitrary: Arbitrary[A], pp1: A => Pretty)
  : PropertyTest[F] =
    PropertyTest {
      for {
        prms0 <- Kleisli.ask[F, Parameters]
        (prms, seed) <- Kleisli.liftF(Sync[F].delay(Prop.startSeed(prms0)))
        genResult <- Kleisli.liftF(Sync[F].delay(arbitrary.arbitrary.doApply(prms, seed)))
        a <- genResult.retrieve
          .map(executeForArg(test, Prop.slideSeed(prms0), genResult))
          .getOrElse(Kleisli.pure[F, Parameters, Prop.Result](Prop.undecided(prms)))
      } yield a
    }
}

object ForAllShrink
{
  sealed trait ShrinkResult[A]

  object ShrinkResult
  {
    case class Success[A](result: Prop.Result)
    extends ShrinkResult[A]

    case class FirstFailure[A](value: A, result: Prop.Result)
    extends ShrinkResult[A]
  }

  def executeForArg[F[_]: Sync, A, P]
  (test: A => PropertyTest[F])
  (value: A)
  : Kleisli[F, Parameters, Prop.Result] =
    test(value).test
      .recoverWith { case e: Throwable => Kleisli.pure(Prop.Result(status = Prop.Exception(e))) }
      .map(ForAll.provedToTrue)

  def executeForArgWithSliddenSeed[F[_]: Sync, A, P]
  (test: A => PropertyTest[F], prms0: Parameters)
  (value: A)
  : Kleisli[F, Parameters, Prop.Result] =
    for {
      prms <- Kleisli.liftF(Sync[F].delay(Prop.slideSeed(prms0)))
      result <- Kleisli.local((_: Parameters) => prms)(executeForArg(test)(value))
    } yield result

  def firstFailureOrSuccess[F[_]: Sync, A]
  (test: A => PropertyTest[F], prms0: Parameters)
  (values: Stream[PropertyTest.K[F, ?], A])
  : Pull[PropertyTest.K[F, ?], ShrinkResult[A], Unit] = {
    def spin
    (firstFailure: Option[ShrinkResult[A]])
    (in: Stream[PropertyTest.K[F, ?], A])
    : Pull[PropertyTest.K[F, ?], ShrinkResult[A], Unit] =
      in.pull.uncons1.flatMap {
        case Some((value, tail)) =>
          for {
            result <- Pull.eval(executeForArgWithSliddenSeed(test, prms0)(value))
            _ <- {
              if (result.failure) spin(firstFailure.orElse(Some(ShrinkResult.FirstFailure(value, result))))(tail)
              else Pull.output1(ShrinkResult.Success[A](result)) >> Pull.done
            }
          } yield ()
        case None => firstFailure.traverse(Pull.output1) >> Pull.done
      }
    spin(None)(values)
  }

  def updateResult(r0: Prop.Result, r1: Prop.Result): Prop.Result =
    (r0.args,r1.args) match {
      case (a0 :: _, a1 :: as) =>
        r1.copy(
          args = a1.copy(
            origArg = a0.origArg,
            prettyOrigArg = a0.prettyOrigArg
          ) :: as
        )
      case _ => r1
    }

  def shrinker[F[_]: Sync, A]
  (test: A => PropertyTest[F], prms: Parameters, genResult: Gen.R[A], labels: String)
  (shrinks: Int, value: A, initialValue: A, previousResult: Prop.Result)
  (implicit arbitrary: Arbitrary[A], shrink: Shrink[A], pp1: A => Pretty)
  : Kleisli[F, Parameters, Prop.Result] =
  {
    val res = ForAll.addArg(labels)(value, initialValue, 0)(previousResult)
    Stream.unfold(Shrink.shrink[A](value).filter(genResult.sieve))(as => as.headOption.map(a => (a, as.drop(1))))
      .through(a => firstFailureOrSuccess[F, A](test, prms)(a).stream)
      .compile
      .last
      .flatMap {
        case Some(ShrinkResult.FirstFailure(failedValue, failedResult)) =>
          val newResult = updateResult(previousResult, failedResult)
          shrinker(test, prms, genResult, labels)(shrinks + 1, failedValue, initialValue, newResult)
        case Some(ShrinkResult.Success(result)) => PropertyTest.kleisli[F].pure(result)
        case None => PropertyTest.kleisli[F].pure(res)
      }
  }

  def executeAndShrink[F[_]: Sync, A, P]
  (test: A => PropertyTest[F], prms0: Parameters, genResult: Gen.R[A], labels: String)
  (value: A)
  (implicit arbitrary: Arbitrary[A], shrink: Shrink[A], pp1: A => Pretty)
  : Kleisli[F, Parameters, Prop.Result] =
    for {
      result <- executeForArgWithSliddenSeed(test, prms0)(value)
      finalResult <- {
        if (result.failure) shrinker(test, prms0, genResult, labels)(0, value, value, result)
        else PropertyTest.kleisli[F]
          .pure(ForAll.addArg(labels)(value, value, 0)(result))
      }
    } yield finalResult

  def apply[F[_]: Sync, A, P]
  (test: A => PropertyTest[F])
  (implicit arbitrary: Arbitrary[A], shrink: Shrink[A], pp1: A => Pretty)
  : PropertyTest[F] =
    PropertyTest {
      for {
        prms0 <- Kleisli.ask[F, Parameters]
        (prms, seed) <- Kleisli.liftF(Sync[F].delay(Prop.startSeed(prms0)))
        genResult <- Kleisli.liftF(Sync[F].delay(arbitrary.arbitrary.doApply(prms, seed)))
        a <- genResult.retrieve
          .map(executeAndShrink(test, prms0, genResult, genResult.labels.mkString(",")))
          .getOrElse(PropertyTest.kleisli[F].pure(Prop.undecided(prms)))
      } yield a
    }
}
