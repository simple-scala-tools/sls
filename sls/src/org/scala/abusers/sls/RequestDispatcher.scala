package org.scala.abusers.sls

import cats.effect.kernel.Deferred
import cats.effect.kernel.Fiber
import cats.effect.kernel.Ref
import cats.effect.kernel.Resource
import cats.effect.std.MapRef
import cats.effect.std.Queue
import cats.effect.IO
import cats.syntax.all._
import fs2.concurrent.SignallingRef
import langoustine.lsp.{PreparedNotification => _, PreparedRequest => _, *}
import langoustine.lsp.requests.*
import langoustine.lsp.runtime.*
import langoustine.lsp.structures.*

import scala.concurrent.duration._

import _root_.fs2.Pipe
import _root_.fs2.Stream

trait RequestDispatcher {

  def handleCompletion(in: Invocation[CompletionParams, IO]): IO[textDocument.completion.Out]

  def handleDidChange(in: Invocation[DidChangeTextDocumentParams, IO]): IO[Unit]

  def handleInitialized(in: Invocation[InitializedParams, IO]): IO[Unit]
}

case class State()

type CommandResult[T]   = IO[(Option[State], T)]
type NotificationResult = Stream[IO, Option[State]]

trait ServerImpl {
  def handleCompletion(
      in: (State, Invocation[CompletionParams, IO])
  ): CommandResult[Opt[CompletionList]]

  def handleNotification(in: Stream[IO, (State, PreparedNotification[IO])]): NotificationResult
}

sealed trait AsyncRequest[F[_]]

trait PreparedRequest[F[_]] extends AsyncRequest[F] {
  type X <: LSPRequest
  type Out = x.Out
  val x: X
  val in: Invocation[x.In, F]
  val replyTo: Deferred[F, Out]

  def toFull: PreparedRequest.Full[X, F] = PreparedRequest.Full(x, in, replyTo)
}
object PreparedRequest {

  case class Full[X <: LSPRequest, F[_]](
      x: X,
      in: Invocation[x.In, F],
      replyTo: Deferred[F, x.Out],
  ) extends AsyncRequest[F]

  def apply[X2 <: LSPRequest, F[_]](_x: X2, _in: Invocation[_x.In, F], _replyTo: Deferred[F, _x.Out]) =
    new PreparedRequest[F] {
      type X = _x.type
      val x       = _x
      val in      = _in
      val replyTo = _replyTo
    }

  def apply[X2 <: LSPRequest, F[_]](x: X2) = new UnapplyStage[F](x)

  class UnapplyStage[F[_]](target: LSPRequest) {
    def unapply(prep: PreparedRequest[F]): Option[(Invocation[prep.x.In, F], Deferred[F, prep.x.Out])] =
      if prep.x == target then Some((prep.in, prep.replyTo))
      else None
  }

}

trait PreparedNotification[F[_]] extends AsyncRequest[F] {
  type X <: LSPNotification
  type In = x.In
  val x: X
  val in: Invocation[x.In, F]

}

object PreparedNotification {

  def apply[X2 <: LSPNotification, F[_]](_x: X2, _in: Invocation[_x.In, F]) = new PreparedNotification[F] {
    type X = _x.type
    val x  = _x
    val in = _in
  }
}

extension [X <: LSPRequest](r: X) {
  def matcher[F[_]]: Matcher[X, F] = new Matcher[X, F](r)
}

class Matcher[X <: LSPRequest, F[_]](val expected: X) {
  def unapply(req: PreparedRequest.Full[X, F]): Option[(Invocation[expected.In, F], Deferred[F, expected.Out])] =
    if req.x == expected then Some(
      (req.in.asInstanceOf[Invocation[expected.In, F]], req.replyTo.asInstanceOf[Deferred[F, expected.Out]]) // TODO
    )
    else None
}

object RequestDispatcher {

  def start(server: ServerImpl, stateRef: Ref[IO, State]): Resource[IO, RequestDispatcher] =
    for {
      changeQueue <- Resource.eval(Queue.unbounded[IO, AsyncRequest[IO]])
      _ <-
        fs2.Stream
          .fromQueueUnterminated(changeQueue)
          .evalMap(a => stateRef.get.map(s => (s, a)))
          .broadcastThrough(
            _.collect { a =>
              (a: @unchecked) match {
                case (s, r: PreparedNotification[IO]) => (s, r)
              }
            }
              .through(server.handleNotification),
            _.collect { a =>
              (a: @unchecked) match {
                case (s, r: PreparedRequest[IO]) => (s, r.toFull)
              }
            }.evalMap { a =>
              val pattern = textDocument.completion.matcher[IO]
              a match {
                case (s, pattern(in, out)) =>
                  server
                    .handleCompletion(s -> in)
                    .flatMap { case (s2, r) => out.complete(r).as(s2) }
                case other => sys.error(other.toString)
              }
            },
          )
          .evalMap {
            case Some(state) => stateRef.set(state)
            case None        => IO.unit
          }
          .compile
          .drain
          .background

    } yield new RequestDispatcher {

      def handleCompletion(in: Invocation[CompletionParams, IO]): IO[textDocument.completion.Out] =
        for {
          reply  <- Deferred[IO, textDocument.completion.Out]
          _      <- changeQueue.offer(PreparedRequest(textDocument.completion, in, reply))
          result <- reply.get
        } yield result

      def handleDidChange(in: Invocation[DidChangeTextDocumentParams, IO]): IO[Unit] =
        changeQueue.offer(PreparedNotification(textDocument.didChange, in))

      def handleInitialized(in: Invocation[InitializedParams, IO]): IO[Unit] =
        changeQueue.offer(PreparedNotification(initialized, in))
    }
}
