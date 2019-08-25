package org.scalacheck

import cats.data.Kleisli
import cats.effect.Sync
import cats.implicits._
import klk.PropertyTest
import org.scalacheck.Gen.Parameters
import org.scalacheck.util.Pretty

object ForAll
{
  def provedToTrue(r: Prop.Result) = r.status match {
    case Prop.Proof => r.copy(status = Prop.True)
    case _ => r
  }

  def cont[F[_]: Sync, A, P]
  (f: A => PropertyTest[F], prms: Parameters, genResult: Gen.R[A])
  (value: A)
  (implicit pp1: A => Pretty)
  : Kleisli[F, Parameters, Prop.Result] =
    Kleisli.local((_: Parameters) => prms)(f(value).test)
      .recoverWith { case e: Throwable => Kleisli.pure(Prop.Result(status = Prop.Exception(e))) }
      .map(provedToTrue)
      .map(_.addArg(Prop.Arg(genResult.labels.mkString(","), value, 0, value, pp1(value), pp1(value))))

  def noShrink[F[_]: Sync, A, P, Output]
  (f: A => PropertyTest[F])
  (implicit arbitrary: Arbitrary[A], pp1: A => Pretty)
  : PropertyTest[F] =
    PropertyTest {
      for {
        prms0 <- Kleisli.ask[F, Parameters]
        (prms, seed) <- Kleisli.liftF(Sync[F].delay(Prop.startSeed(prms0)))
        genResult <- Kleisli.liftF(Sync[F].delay(arbitrary.arbitrary.doApply(prms, seed)))
        a <- genResult.retrieve
          .map(cont(f, Prop.slideSeed(prms0), genResult))
          .getOrElse(Kleisli.pure[F, Parameters, Prop.Result](Prop.undecided(prms)))
      } yield a
    }
}
