package org.scala.abusers.sls // TODO package completions are still here when they should not, also we should get whole package completion out of the box

import cats.effect.*
import cats.effect.std.MapRef
import cats.parse.LocationMap
import cats.syntax.all.*
import langoustine.lsp.aliases.TextDocumentContentChangeEvent
import langoustine.lsp.structures.*
import langoustine.lsp.Invocation

import java.net.URI

case class DocumentState(content: String):
  private lazy val locationMap = LocationMap(content)

  extension (lspPos: Position)
    def toOffset: Int =
      locationMap.toOffset(lspPos.line.value, lspPos.character.value).getOrElse(-1 /* no such line */ )

  def processEdits(edits: Vector[TextDocumentContentChangeEvent]): DocumentState =
    edits.toList
      .foldLeft(this):
        case (_, incremental: TextDocumentContentChangeEvent.S0) => applyEdit(incremental)
        case (_, full: TextDocumentContentChangeEvent.S1)        => DocumentState(full.text)
        case _                                                   => sys.error("Illegal State Exception")

  private def applyEdit(edit: TextDocumentContentChangeEvent.S0): DocumentState =
    val Range(startPos, endPos) = edit.range
    val startOffset             = startPos.toOffset
    val endOffset               = endPos.toOffset
    val init                    = content.take(startOffset)
    val end                     = content.drop(endOffset)
    DocumentState(init ++ edit.text ++ end)

object DocumentSyncManager:
  def instance: IO[DocumentSyncManager] =
    MapRef.ofScalaConcurrentTrieMap[IO, URI, DocumentState].map(DocumentSyncManager.apply)

class DocumentSyncManager(val documents: MapRef[IO, URI, Option[DocumentState]]):
  import LspNioConverter.*

  def didOpen(in: Invocation[DidOpenTextDocumentParams, IO]): IO[Unit] =
    getOrCreateDocument(in.params.textDocument.uri.asNio, in.params.textDocument.text.some).void

  def didChange(in: Invocation[DidChangeTextDocumentParams, IO]) =
    onTextEditReceived(in.params.textDocument.uri.asNio, in.params.contentChanges)

  def didClose(in: Invocation[DidCloseTextDocumentParams, IO]): IO[Unit] =
    documents.unsetKey(in.params.textDocument.uri.asNio)

  def didSave(in: Invocation[DidSaveTextDocumentParams, IO]): IO[Unit] =
    getOrCreateDocument(in.params.textDocument.uri.asNio, in.params.text.toOption).void

  private def onTextEditReceived(uri: URI, edits: Vector[TextDocumentContentChangeEvent]): IO[Unit] =
    for
      _ <- getOrCreateDocument(uri, None)
      _ <- documents.updateKeyValueIfSet(uri, _.processEdits(edits))
    yield ()

  def get(uri: URI): IO[DocumentState] =
    documents(uri).get.flatMap(IO.fromOption(_)(IllegalStateException()))

  private def getOrCreateDocument(uri: URI, content: Option[String]): IO[DocumentState] =
    val content0 = content.getOrElse("")
    documents(uri)
      .updateAndGet:
        case Some(existing) => Some(existing)
        case None           => Some(new DocumentState(content0))
      .map(_.get)
