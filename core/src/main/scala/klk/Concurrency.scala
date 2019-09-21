package klk

import java.util.concurrent.{ExecutorService, Executors}

import scala.concurrent.ExecutionContext

import cats.effect.{ContextShift, IO, Resource, Sync}
import fs2.Stream

object Concurrency
{
  val defaultNum: Int =
    Runtime.getRuntime.availableProcessors

  def fixedPoolWith[F[_]: Sync](num: Int): F[ExecutorService] =
    Sync[F].delay(Executors.newFixedThreadPool(num))

  def ec[F[_]: Sync](pool: F[ExecutorService]): Resource[F, ExecutionContext] =
    Resource.make(pool)(es => Sync[F].delay(es.shutdown()))
      .map(ExecutionContext.fromExecutorService)

  def fixedPool[F[_]: Sync]: F[ExecutorService] =
    fixedPoolWith(defaultNum)

  def fixedPoolEc: Resource[IO, ExecutionContext] =
    ec(fixedPool[IO])

  def fixedPoolEcWith(num: Int): Resource[IO, ExecutionContext] =
    ec(fixedPoolWith[IO](num))

  def fixedPoolEcStream: Stream[IO, ExecutionContext] =
    Stream.resource(fixedPoolEc)

  def cs(pool: IO[ExecutorService]): Resource[IO, ContextShift[IO]] =
    ec(pool)
      .map(IO.contextShift(_))

  def fixedPoolCsWith(num: Int): Resource[IO, ContextShift[IO]] =
    cs(fixedPoolWith[IO](num))

  def fixedPoolCs: Resource[IO, ContextShift[IO]] =
    cs(fixedPool[IO])

  def fixedPoolCsStreamWith(num: Int): Stream[IO, ContextShift[IO]] =
    Stream.resource(fixedPoolCsWith(num))

  def fixedPoolCsStream: Stream[IO, ContextShift[IO]] =
    Stream.resource(fixedPoolCs)
}
