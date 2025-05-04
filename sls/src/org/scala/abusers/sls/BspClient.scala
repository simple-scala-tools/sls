package org.scala.abusers.sls

import bsp.*
import cats.effect.*
import com.comcast.ip4s.*
import fs2.io.*
import fs2.io.net.Network
import jsonrpclib.fs2.*
import jsonrpclib.smithy4sinterop.ClientStub

def makeBspClient(path: String, channel: FS2Channel[IO], report: String => IO[Unit]): Resource[IO, BuildServer[IO]] =
  Network[IO]
    .connect(UnixSocketAddress(path))
    .flatMap { socket =>
      Resource.make(for
        client <- ClientStub(BuildServer, channel)
        fiber <- fs2.Stream
          .eval(IO.never)
          .concurrently(
            socket.reads.through(lsp.decodeMessages).evalTap(m => report(m.toString)).through(channel.inputOrBounce)
          )
          .concurrently(channel.output.through(lsp.encodeMessages).through(socket.writes))
          .compile
          .drain
          .guarantee(IO.consoleForIO.errorln("Terminating server"))
          .start
      yield (client, fiber)) { case (_, fiber) => fiber.cancel }
    }
    .map { case (client, _) => client }
