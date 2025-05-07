package org.scala.abusers.sls

import bsp.BuildServer
import cats.data.OptionT
import cats.effect.*
import cats.syntax.all.*
import jsonrpclib.fs2.catsMonadic
import langoustine.lsp.*
import langoustine.lsp.all.*
import langoustine.lsp.app.*
import org.scala.abusers.pc.IOCancelTokens
import org.scala.abusers.pc.PresentationCompilerProvider

case class State(files: Set[String], bloopConn: Option[BloopConnection]):
  def release: IO[Unit]                          = OptionT.fromOption[IO](bloopConn).semiflatTap(_.cancel).value.void
  def withBloopConnection(conn: BloopConnection) = copy(bloopConn = Some(conn))

case class BloopConnection(client: BuildServer[IO], cancel: IO[Unit])

object SimpleScalaServer extends LangoustineApp:

  override def server(args: List[String]): Resource[IO, LSPBuilder[IO]] =
    Resource
      .make(IO.ref(State(Set.empty, none)))(ref =>
        ref
          .getAndUpdate(_.copy(bloopConn = None))
          .map(_.release)
      )
      .flatMap(myLSP(using _))
      .onFinalizeCase(s => IO.consoleForIO.errorln(s"closing with $s"))

  private def myLSP(using stateRef: Ref[IO, State]): Resource[IO, LSPBuilder[IO]] =

    for
      textDocumentSync <- DocumentSyncManager.instance.toResource
      pcProvider       <- PresentationCompilerProvider.instance.toResource
      inverseSource <-
        InverseSourcesToTarget.instance.toResource // move into bsp state manager // possible performance improvement to constant asking for inverse sources, leaving it for now
      cancelTokens <- IOCancelTokens.instance
      impl = ServerImpl(textDocumentSync, pcProvider, inverseSource, cancelTokens)
    yield LSPBuilder
      .create[IO]
      .handleRequest(initialize)(impl.handleInitialize)
      .handleNotification(textDocument.didOpen)(impl.handleDidOpen)
      .handleNotification(textDocument.didClose)(impl.handleDidClose)
      .handleNotification(textDocument.didChange)(impl.handleDidChange)
      .handleRequest(textDocument.completion)(impl.handleCompletion)
      .handleNotification(textDocument.didSave)(impl.handleDidSave)
