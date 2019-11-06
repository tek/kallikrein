package klk

import java.util.concurrent.TimeUnit

import cats.Eval
import cats.effect.{Clock, Sync}

trait MeasureTest[F[_]]
{
  def apply[A](thunk: F[A]): F[(A, Long)]
}

object MeasureTest
{
  implicit def MeasureTest_Sync[F[_]: Sync]: MeasureTest[F] =
    new MeasureTest[F] {
      val clock: Clock[F] = Clock.create[F]

      def now: F[Long] =
        clock.realTime(TimeUnit.MILLISECONDS)

      def apply[A](thunk: F[A]): F[(A, Long)] =
        for {
          startTime <- now
          result <- thunk
          endTime <- now
        } yield (result, endTime - startTime)
    }

  implicit def Measure_Eval: MeasureTest[Eval] =
    new MeasureTest[Eval] {
      def now: Eval[Long] =
        Eval.always(System.currentTimeMillis())

      def apply[A](thunk: Eval[A]): Eval[(A, Long)] =
        for {
          startTime <- now
          result <- thunk
          endTime <- now
        } yield (result, endTime - startTime)
    }
}
