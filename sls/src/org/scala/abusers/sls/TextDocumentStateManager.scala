package org.scala.abusers.sls // TODO also we should get whole package completion out of the box

import cats.effect.*
import cats.effect.std.AtomicCell
import cats.parse.LocationMap
import cats.syntax.all.*
import langoustine.lsp.aliases.TextDocumentContentChangeEvent
import langoustine.lsp.structures.*
import langoustine.lsp.Invocation

import java.net.URI

case class DocumentState(content: String, uri: URI) {
  private lazy val locationMap = LocationMap(content)

  extension (lspPos: Position) {
    def toOffset: Int =
      locationMap.toOffset(lspPos.line.value, lspPos.character.value).getOrElse(-1 /* no such line */ )
  }

  def processEdits(edits: Vector[TextDocumentContentChangeEvent]): DocumentState =
    edits.toList
      .foldLeft(this) {
        case (_, incremental: TextDocumentContentChangeEvent.S0) => applyEdit(incremental)
        case (_, full: TextDocumentContentChangeEvent.S1)        => DocumentState(full.text, uri)
        case _                                                   => sys.error("Illegal State Exception")
      }

  private def applyEdit(edit: TextDocumentContentChangeEvent.S0): DocumentState = {
    val Range(startPos, endPos) = edit.range
    val startOffset             = startPos.toOffset
    val endOffset               = endPos.toOffset
    val init                    = content.take(startOffset)
    val end                     = content.drop(endOffset)
    DocumentState(init ++ edit.text ++ end, uri)
  }
}

object TextDocumentSyncManager {
  def instance: IO[TextDocumentSyncManager] =
    AtomicCell[IO].of(Map[URI, DocumentState]()).map(TextDocumentSyncManager(_))
}

class TextDocumentSyncManager(val documents: AtomicCell[IO, Map[URI, DocumentState]]) {
  import NioConverter.*

  def didOpen(in: Invocation[DidOpenTextDocumentParams, IO]): IO[Unit] =
    getOrCreateDocument(in.params.textDocument.uri.asNio, in.params.textDocument.text.some).void

  def didChange(in: Invocation[DidChangeTextDocumentParams, IO]) =
    onTextEditReceived(in.params.textDocument.uri.asNio, in.params.contentChanges)

  def didClose(in: Invocation[DidCloseTextDocumentParams, IO]): IO[Unit] =
    documents.update(_.removed(in.params.textDocument.uri.asNio))

  def didSave(in: Invocation[DidSaveTextDocumentParams, IO]): IO[Unit] =
    getOrCreateDocument(in.params.textDocument.uri.asNio, in.params.text.toOption).void

  private def onTextEditReceived(uri: URI, edits: Vector[TextDocumentContentChangeEvent]): IO[Unit] =
    for {
      doc <- getOrCreateDocument(uri, None)
      _   <- documents.update(_.updated(uri, doc.processEdits(edits)))
    } yield ()

  def get(uri: URI): IO[DocumentState] =
    documents.get.map(_.get(uri)).flatMap(IO.fromOption(_)(IllegalStateException()))

  private def getOrCreateDocument(uri: URI, content: Option[String]): IO[DocumentState] =
    documents.modify { access =>
      access.get(uri) match {
        case Some(doc) => access -> doc
        case None =>
          val newDoc = new DocumentState(content.getOrElse(""), uri)
          access.updated(uri, newDoc) -> newDoc
      }
    }
}
