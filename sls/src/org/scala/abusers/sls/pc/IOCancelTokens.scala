package org.scala.abusers.pc

import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import cats.effect.IO
import cats.syntax.all.*

import java.util.concurrent.CancellationException
import java.util.concurrent.CompletionStage
import scala.meta.pc.CancelToken

trait IOCancelTokens:
  def withCancelToken[A](f: CancelToken => IO[A]): IO[A]

object IOCancelTokens:
  def instance: Resource[IO, IOCancelTokens] = Dispatcher.parallel[IO].map { dispatcher =>
    new:
      def withCancelToken[A](f: CancelToken => IO[A]): IO[A] =
        (IO.deferred[Unit], IO.deferred[Unit]).flatMapN { (completed, cancelRequested) =>

          val token: CancelToken = new:
            def checkCanceled(): Unit =
              if dispatcher.unsafeRunSync(cancelRequested.tryGet.map(_.isDefined))
              then throw new CancellationException()

            def onCancel(): CompletionStage[java.lang.Boolean] =
              dispatcher.unsafeToCompletableFuture(
                completed.get
                  .race(cancelRequested.get)
                  .map(_.isRight)
              )

          f(token)
            .cancelable(cancelRequested.complete(()).void)
            .guarantee(completed.complete(()).void)
        }

  }
