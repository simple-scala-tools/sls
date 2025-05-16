package org.scala.abusers.sls

import bsp.BuildClient
import bsp.DidChangeBuildTarget
import bsp.LogMessageParams
import bsp.OnBuildTaskFinishInput
import bsp.OnBuildTaskStartInput
import bsp.PrintParams
import bsp.PublishDiagnosticsParams
import bsp.ShowMessageParams
import bsp.TaskProgressParams
import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.*
import fs2.io.net.Network
import jsonrpclib.fs2.*
import jsonrpclib.Endpoint
import langoustine.lsp.requests.window
import langoustine.lsp.Communicate
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

def bspClientHandler(lspClient: Communicate[IO]): List[Endpoint[IO]] = BSPCodecs.serverEndpoints(
  new BuildClient[IO]:
    private def notify(msg: String) =
      lspClient.notification(
        window.showMessage(
          langoustine.lsp.structures
            .ShowMessageParams(`type` = langoustine.lsp.enumerations.MessageType.Info, message = msg)
        )
      )

    def onBuildLogMessage(input: LogMessageParams): IO[Unit] = notify(
      s"handling onBuildLogMessage: $input"
    )

    def onBuildPublishDiagnostics(input: PublishDiagnosticsParams): IO[Unit] = notify(
      s"handling onBuildPublishDiagnostics: $input"
    )

    def onBuildShowMessage(input: ShowMessageParams): IO[Unit] =
      notify(
        s"handling onBuildShowMessage: $input"
      )

    def onBuildTargetDidChange(input: DidChangeBuildTarget): IO[Unit] = notify(
      s"handling onBuildTargetDidChange: $input"
    )

    def onBuildTaskFinish(input: OnBuildTaskFinishInput): IO[Unit] = notify(
      s"handling onBuildTaskFinish: $input"
    )

    def onBuildTaskProgress(input: TaskProgressParams): IO[Unit] = notify(
      s"handling onBuildTaskProgress: $input"
    )

    def onBuildTaskStart(input: OnBuildTaskStartInput): IO[Unit] = notify(
      s"handling onBuildTaskStart: $input"
    )

    def onRunPrintStderr(input: PrintParams): IO[Unit] = notify(
      s"handling onRunPrintStderr: $input"
    )

    def onRunPrintStdout(input: PrintParams): IO[Unit] = notify(
      s"handling onRunPrintStdout: $input"
    )
)
