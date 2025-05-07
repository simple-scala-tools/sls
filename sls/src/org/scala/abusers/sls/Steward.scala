package org.scala.abusers.sls

import cats.effect.kernel.Concurrent
import cats.effect.kernel.MonadCancelThrow
import cats.effect.kernel.Ref
import cats.effect.kernel.Resource
import cats.effect.kernel.Resource.ExitCase
import cats.syntax.all.*

trait Steward[F[_]]:
  def acquire[A](resource: Resource[F, A]): F[A]

object Steward:
  def apply[F[_]: Concurrent]: Resource[F, Steward[F]] = Resource
    .makeCase(Ref.of(List.empty[ExitCase => F[Unit]])) { (finalizers, exit) =>
      finalizers.get.flatMap(_.traverse_(_(exit)))
    }
    .map(Impl(_))

  private case class Impl[F[_]: MonadCancelThrow](state: Ref[F, List[ExitCase => F[Unit]]]) extends Steward[F]:

    def acquire[A](resource: Resource[F, A]): F[A] =
      MonadCancelThrow[F].uncancelable { poll =>
        poll(resource.allocatedCase).flatMap { case (a, finalizer) =>
          state.update(finalizer :: _).as(a)
        }
      }
