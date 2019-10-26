package klk

import java.util.concurrent.TimeUnit

import cats.effect.{Clock, Sync}

trait MeasureTest[F[_]]
{
  def apply[A](thunk: F[A]): F[(A, Long)]
}

object MeasureTest
{
  implicit def MeasureTest_Sync[F[_]: Sync] =
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
}
