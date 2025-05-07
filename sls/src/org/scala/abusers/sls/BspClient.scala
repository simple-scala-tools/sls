package org.scala.abusers.sls

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.*
import fs2.io.net.Network
import jsonrpclib.fs2.*
import smithy4sbsp.bsp4s.BSPCodecs

def makeBspClient(path: String, channel: FS2Channel[IO], report: String => IO[Unit]): Resource[IO, BuildServer] =
  Network[IO]
    .connect(UnixSocketAddress(path))
    .flatMap { socket =>
      fs2.Stream
        .eval(IO.never)
        .concurrently(
          socket.reads.through(lsp.decodeMessages).evalTap(m => report(m.toString)).through(channel.inputOrBounce)
        )
        .concurrently(channel.output.through(lsp.encodeMessages).through(socket.writes))
        .compile
        .drain
        .guarantee(IO.consoleForIO.errorln("Terminating server"))
        .background
    }
    .as(
      BuildServer(
        BSPCodecs.clientStub(bsp.BuildServer, channel),
        BSPCodecs.clientStub(bsp.jvm.JvmBuildServer, channel),
        BSPCodecs.clientStub(bsp.scala_.ScalaBuildServer, channel),
        BSPCodecs.clientStub(bsp.java_.JavaBuildServer, channel),
      )
    )
