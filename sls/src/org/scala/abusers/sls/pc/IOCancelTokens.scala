package org.scala.abusers.pc

import cats.effect.kernel.Resource
import cats.effect.kernel.Resource.ExitCase
import cats.effect.std.Dispatcher
import cats.effect.IO
import cats.syntax.all.*

import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CompletionStage
import scala.meta.pc.CancelToken

trait IOCancelTokens {
  def mkCancelToken: Resource[IO, CancelToken]
}

object IOCancelTokens {
  def instance: Resource[IO, IOCancelTokens] = Dispatcher.parallel[IO].map { dispatcher =>
    new {
      def mkCancelToken: Resource[IO, CancelToken] =
        (IO.deferred[Unit], IO.deferred[Unit]).tupled.toResource.flatMap { (completed, cancelRequested) =>
          val token: CancelToken = new {
            val onCancelVal: CompletableFuture[java.lang.Boolean] = dispatcher.unsafeToCompletableFuture(
              completed.get
                .race(cancelRequested.get)
                .map(_.isRight)
            )

            def checkCanceled(): Unit =
              if onCancelVal.isCancelled
              then throw new CancellationException()

            def onCancel(): CompletionStage[java.lang.Boolean] =
              onCancelVal
          }

          Resource.makeCase(IO.pure(token)) {
            case (_, ExitCase.Canceled) => cancelRequested.complete(()).void
            case _                      => completed.complete(()).void
          }
        }
    }

  }
}
