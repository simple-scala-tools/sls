package org.scala.abusers.sls

import cats.effect.*
import smithy4s.Service

object SmithySuspend {
  def sus[Alg[_[_, _, _, _, _]]](
      implSus: IO[smithy4s.kinds.FunctorAlgebra[Alg, IO]]
  )(using service: Service[Alg]): service.Impl[IO] =
    service.fromPolyFunction(
      new service.FunctorInterpreter[IO] {
        def apply[I, E, O, SI, SO](op: service.Operation[I, E, O, SI, SO]): IO[O] =
          implSus.flatMap(service.toPolyFunction(_).apply(op))
      }
    )
}
