package org.scala.abusers.sls

import cats.effect.*
import jsonrpclib.fs2.catsMonadic
import langoustine.lsp.*
import langoustine.lsp.all.*
import langoustine.lsp.app.*
import org.scala.abusers.pc.IOCancelTokens
import org.scala.abusers.pc.PresentationCompilerProvider

case class BuildServer(
    generic: bsp.BuildServer[IO],
    jvm: bsp.jvm.JvmBuildServer[IO],
    scala: bsp.scala_.ScalaBuildServer[IO],
    java: bsp.java_.JavaBuildServer[IO],
)

object BuildServer {
  def suspend(client: IO[BuildServer]): BuildServer = BuildServer(
    SmithySuspend.sus(client.map(_.generic)),
    SmithySuspend.sus(client.map(_.jvm)),
    SmithySuspend.sus(client.map(_.scala)),
    SmithySuspend.sus(client.map(_.java)),
  )
}

case class State(files: Set[String], bspClient: Deferred[IO, BuildServer])
// def withBspClient(bspClient: BuildServer) = copy(bspClient = (bspClient))

object SimpleScalaServer extends LangoustineApp {

  override def server(args: List[String]): Resource[IO, LSPBuilder[IO]] =
    (for {
      steward           <- ResourceSupervisor[IO]
      bspClientDeferred <- Deferred[IO, BuildServer].toResource
      state             <- IO.ref(State(Set.empty, bspClientDeferred)).toResource
      lsp               <- myLSP(steward)(using state)
    } yield lsp).onFinalizeCase(s => IO.consoleForIO.errorln(s"closing with $s"))

  private def myLSP(steward: ResourceSupervisor[IO])(using stateRef: Ref[IO, State]): Resource[IO, LSPBuilder[IO]] =

    for {
      textDocumentSync  <- DocumentSyncManager.instance.toResource
      pcProvider        <- PresentationCompilerProvider.instance.toResource
      bspClientDeferred <- Deferred[IO, BuildServer].toResource
      bspStateManager   <- BspStateManager.instance(BuildServer.suspend(bspClientDeferred.get)).toResource
      cancelTokens      <- IOCancelTokens.instance
      impl = ServerImpl(textDocumentSync, pcProvider, bspStateManager, cancelTokens)
    } yield LSPBuilder
      .create[IO]
      .handleRequest(initialize)(impl.handleInitialize(steward, bspClientDeferred))
      .handleNotification(textDocument.didOpen)(impl.handleDidOpen)
      .handleNotification(textDocument.didClose)(impl.handleDidClose)
      .handleNotification(textDocument.didChange)(impl.handleDidChange)
      .handleNotification(textDocument.didSave)(impl.handleDidSave)
      .handleRequest(textDocument.completion)(impl.handleCompletion)
      .handleRequest(textDocument.hover)(impl.handleHover)
      .handleRequest(textDocument.signatureHelp)(impl.handleSignatureHelp)
      .handleRequest(textDocument.definition)(impl.handleDefinition)
      .handleRequest(textDocument.inlayHint)(impl.handleInlayHints)
  // .handleRequest(textDocument.inlayHint.resolve)(impl.handleInlayHintsResolve)
}
