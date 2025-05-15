package org.scala.abusers.sls

import bsp.BuildClient
import bsp.BuildTargetEvent
import bsp.BuildTargetIdentifier
import bsp.Diagnostic
import bsp.Identifier
import bsp.MessageType
import bsp.OriginId
import bsp.TaskFinishParams
import bsp.TaskId
import bsp.TaskProgressData
import bsp.TaskStartParams
import bsp.TextDocumentIdentifier
import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.*
import fs2.io.net.Network
import jsonrpclib.fs2.*
import jsonrpclib.Endpoint
import langoustine.lsp.requests.window
import langoustine.lsp.structures.ShowMessageParams
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

def bspClientHandler(lspClient: Communicate[IO]): List[Endpoint[IO]] = Nil
  // BSPCodecs.serverEndpoints(
  // new BuildClient[IO]:
  //   private def notify(msg: String) =
  //     lspClient.notification(
  //       window.showMessage(ShowMessageParams(`type` = langoustine.lsp.enumerations.MessageType.Info, message = msg))
  //     )

  //   override def onBuildLogMessage(
  //       _type: MessageType,
  //       message: String,
  //       task: Option[TaskId],
  //       originId: Option[OriginId],
  //   ): IO[Unit] = notify(s"handling onBuildLogMessage: $_type, $message, $task, $originId")

  //   def onBuildPublishDiagnostics(
  //       textDocument: TextDocumentIdentifier,
  //       buildTarget: BuildTargetIdentifier,
  //       diagnostics: List[Diagnostic],
  //       reset: Boolean,
  //       originId: Option[OriginId],
  //   ): IO[Unit] =
  //     notify(
  //       s"handling onBuildPublishDiagnostics: $textDocument, $buildTarget, $diagnostics, $reset, $originId"
  //     )

  //   def onBuildShowMessage(
  //       _type: MessageType,
  //       message: String,
  //       task: Option[TaskId],
  //       originId: Option[OriginId],
  //   ): IO[Unit] =
  //     notify(s"handling onBuildShowMessage: $_type, $message, $task, $originId")

  //   def onBuildTargetDidChange(changes: List[BuildTargetEvent]): IO[Unit] =
  //     notify(s"handling onBuildTargetDidChange: $changes")

  //   def onBuildTaskFinish(data: TaskFinishParams): IO[Unit] =
  //     notify(s"handling onBuildTaskFinish: $data")

  //   def onBuildTaskProgress(
  //       taskId: TaskId,
  //       originId: Option[Identifier],
  //       eventTime: Option[bsp.Long],
  //       message: Option[String],
  //       total: Option[bsp.Long],
  //       progress: Option[bsp.Long],
  //       unit: Option[String],
  //       data: Option[TaskProgressData],
  //   ): IO[Unit] =
  //     notify(
  //       s"handling onBuildTaskProgress: $taskId, $originId, $eventTime, $message, $total, $progress, $unit, $data"
  //     )

  //   def onBuildTaskStart(data: TaskStartParams): IO[Unit] =
  //     notify(s"handling onBuildTaskStart: $data")
  //   def onRunPrintStderr(originId: Identifier, message: String, task: Option[TaskId]): IO[Unit] =
  //     notify(s"handling onRunPrintStderr: $originId, $message, $task")

  //   def onRunPrintStdout(originId: Identifier, message: String, task: Option[TaskId]): IO[Unit] =
  //     notify(s"handling onRunPrintStdout: $originId, $message, $task")
// )
