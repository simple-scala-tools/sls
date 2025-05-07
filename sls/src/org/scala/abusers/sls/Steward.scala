package org.scala.abusers.sls

import cats.effect.kernel.Concurrent
import cats.effect.kernel.MonadCancel
import cats.effect.kernel.Ref
import cats.effect.kernel.Resource
import cats.syntax.all.*

trait Steward[F[_]]:
  def acquire[A](resource: Resource[F, A]): F[A]

object Steward:
  def apply[F[_]: Concurrent]: Resource[F, Steward[F]] = Resource
    .make(Ref.of(List.empty[F[Unit]])) { finalizers =>
      finalizers.get.flatMap(_.sequence_)
    }
    .map(Impl(_))

  private type MonadCancelThrow[F[_]] = MonadCancel[F, Throwable]

  private case class Impl[F[_]: MonadCancelThrow](state: Ref[F, List[F[Unit]]]) extends Steward[F]:

    def acquire[A](resource: Resource[F, A]): F[A] =
      resource.allocated.flatMap { case (a, finalizer) =>
        state.update(finalizer :: _).as(a)
      }
