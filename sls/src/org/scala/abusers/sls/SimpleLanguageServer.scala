package org.scala.abusers.sls

import cats.effect.*
import cats.syntax.all.*
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

case class State(files: Set[String], bspClient: Option[BuildServer]):
  def withBspClient(bspClient: BuildServer) = copy(bspClient = Some(bspClient))

object SimpleScalaServer extends LangoustineApp:

  override def server(args: List[String]): Resource[IO, LSPBuilder[IO]] =
    (for
      steward <- ResourceSupervisor[IO]
      state   <- IO.ref(State(Set.empty, none)).toResource
      lsp     <- myLSP(steward)(using state)
    yield lsp).onFinalizeCase(s => IO.consoleForIO.errorln(s"closing with $s"))

  private def myLSP(steward: ResourceSupervisor[IO])(using stateRef: Ref[IO, State]): Resource[IO, LSPBuilder[IO]] =

    for
      textDocumentSync <- DocumentSyncManager.instance.toResource
      pcProvider       <- PresentationCompilerProvider.instance.toResource
      inverseSource <-
        InverseSourcesToTarget.instance.toResource // move into bsp state manager // possible performance improvement to constant asking for inverse sources, leaving it for now
      cancelTokens <- IOCancelTokens.instance
      impl = ServerImpl(textDocumentSync, pcProvider, inverseSource, cancelTokens)
    yield LSPBuilder
      .create[IO]
      .handleRequest(initialize)(impl.handleInitialize(steward))
      .handleNotification(textDocument.didOpen)(impl.handleDidOpen)
      .handleNotification(textDocument.didClose)(impl.handleDidClose)
      .handleNotification(textDocument.didChange)(impl.handleDidChange)
      .handleNotification(textDocument.didSave)(impl.handleDidSave)
      .handleRequest(textDocument.completion)(impl.handleCompletion)
