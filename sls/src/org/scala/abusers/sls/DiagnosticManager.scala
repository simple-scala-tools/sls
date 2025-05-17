package org.scala.abusers.sls

import cats.effect.std.MapRef
import cats.effect.IO
import langoustine.lsp.structures.DidChangeTextDocumentParams
import langoustine.lsp.Invocation
import langoustine.lsp.structures.PublishDiagnosticsParams as LspPublishDiagnosticsParams
import langoustine.lsp.structures.Diagnostic as LspDiagnostic
import langoustine.lsp.requests.textDocument.publishDiagnostics
import langoustine.lsp
import langoustine.lsp.runtime.*
import bsp.PublishDiagnosticsParams as BspPublishDiagnosticsParams
import bsp.Diagnostic as BspDiagnostic
import org.scala.abusers.sls.NioConverter.*
import cats.syntax.all.*
import java.net.URI
import langoustine.lsp.Communicate
import smithy4s.json.Json

/**
  * Diagnostic State Manager
  *
  * This class is reposnobile for holding the state of the diagnostic displayed on the client
  *
  * Proposed heuristic is:
  *   On file save we trigger compilation, and this results in notifications being sent from BSP server.
  *   We will keep adding diagnostics, and will clean them only when [[PublishDiagnosticsParams.reset]]
  *   Is
  *
  * @param publishedDiagnostics
  */
class DiagnosticManager(publishedDiagnostics: MapRef[IO, URI, Option[Set[LspDiagnostic]]]) {
  private def convertDiagnostic(bspDiag: BspDiagnostic): LspDiagnostic = {
    val data = Json.writeBlob(bspDiag)
    upickle.default.read[LspDiagnostic](data.asByteBuffer)
  }

  def didChange(in: Invocation[DidChangeTextDocumentParams, IO]): IO[Unit] = {
    // remove diagnostic on modified lines
    // ask presentation compiler for diagnostics
    IO.unit
  }

  def onBuildPublishDiagnostics(lspClient: Communicate[IO], input: BspPublishDiagnosticsParams): IO[Unit] = {
    val bspUri = input.textDocument.uri
    val lspDiags = input.diagnostics.toSet.map(convertDiagnostic)
    def request(diags: Set[LspDiagnostic]) =
      LspPublishDiagnosticsParams(DocumentUri(bspUri.value), Opt.empty, diags.toVector)
    if input.reset then {
      for {
        _ <- publishedDiagnostics(input.textDocument.uri.asNio).set(lspDiags.some)
        _ <- lspClient.notification(publishDiagnostics(request(lspDiags)))
      } yield ()

    } else {
      for {
        currentDiags <- publishedDiagnostics(bspUri.asNio).updateAndGet(_.foldLeft(lspDiags)(_ ++ _).some)
        _ <- lspClient.notification(publishDiagnostics(request(currentDiags.get)))
      } yield ()
    }
  }
}

object DiagnosticManager {
  def instance: IO[DiagnosticManager] = {
    MapRef.ofScalaConcurrentTrieMap[IO, URI, Set[LspDiagnostic]].map(DiagnosticManager.apply)
  }
}
