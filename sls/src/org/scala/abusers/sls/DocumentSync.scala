package org.scala.abusers.sls // TODO package completions are still here when they should not, also we should get whole package completion out of the box

import cats.effect.*
import cats.syntax.all.*
import langoustine.lsp.aliases.TextDocumentContentChangeEvent
import langoustine.lsp.structures.*
import langoustine.lsp.Invocation

import java.net.URI
import scala.annotation.switch
import scala.collection.mutable.ArrayBuffer

object Chars:
  inline val LF = '\u000A'
  inline val FF = '\u000C'
  inline val CR = '\u000D'
  inline val SU = '\u001A'

  /** Is character a line break? */
  def isLineBreakChar(c: Char): Boolean = (c: @switch) match
    case LF | FF | CR | SU => true
    case _                 => false

case class DocumentState(content: Ref[IO, String]):

  private def calculateLineIndicesFromContents(cs: String) =
    val buf = new ArrayBuffer[Int]
    buf += 0
    var i = 0
    while i < cs.length do
      val isLineBreak =
        val ch = cs(i)
        // don't identify the CR in CR LF as a line break, since LF will do.
        if ch == Chars.CR then i + 1 == cs.length || cs(i + 1) != Chars.LF
        else Chars.isLineBreakChar(ch)
      if isLineBreak then buf += i + 1
      i += 1
    buf += cs.length // sentinel, so that findLine below works smoother
    buf.toArray

  /** Map line to offset of first character in line */
  def lineToOffset(index: Int, cs: String): Int = calculateLineIndicesFromContents(cs)(index)

  def processEdits(edits: Vector[TextDocumentContentChangeEvent]): IO[Unit] =
    edits.toList
      .map:
        case incremental: TextDocumentContentChangeEvent.S0 => applyEdit(incremental)
        case full: TextDocumentContentChangeEvent.S1        => content.set(full.text)
      .traverse(identity)
      .void

  private def applyEdit(edit: TextDocumentContentChangeEvent.S0): IO[Unit] =
    content.update: doc =>
        val Range(startPos, endPos) = edit.range
        val startOffset             = lineToOffset(startPos.line.value, doc) + startPos.character.value
        val endOffset               = lineToOffset(endPos.line.value, doc) + endPos.character.value
        val init                    = doc.take(startOffset)
        val end                     = doc.drop(endOffset)

        init ++ edit.text ++ end

  def getContent: IO[String] = content.get

object DocumentSyncManager:
  def instance: IO[DocumentSyncManager] =
    Ref.of[IO, Map[URI, DocumentState]](Map.empty).map(DocumentSyncManager(_))

class DocumentSyncManager(val documents: Ref[IO, Map[URI, DocumentState]]):
  import LspNioConverter.*

  def didOpen(in: Invocation[DidOpenTextDocumentParams, IO]): IO[Unit] =
    getOrCreateDocument(in.params.textDocument.uri.asNio, in.params.textDocument.text.some).void

  def didChange(in: Invocation[DidChangeTextDocumentParams, IO]) =
    onTextEditReceived(in.params.textDocument.uri.asNio, in.params.contentChanges)

  def didClose(in: Invocation[DidCloseTextDocumentParams, IO]): IO[Unit] =
    documents.update(_.removed(in.params.textDocument.uri.asNio))

  def didSave(in: Invocation[DidSaveTextDocumentParams, IO]): IO[Unit] =
    getOrCreateDocument(in.params.textDocument.uri.asNio, in.params.text.toOption).void

  private def onTextEditReceived(uri: URI, edits: Vector[TextDocumentContentChangeEvent]): IO[Unit] =
    for
      doc <- getOrCreateDocument(uri, None)
      _   <- doc.processEdits(edits)
    yield ()

  def get(uri: URI): IO[DocumentState] =
    documents.get.flatMap(docs => IO.fromOption(docs.get(uri))(IllegalStateException()))

  private def getOrCreateDocument(uri: URI, content: Option[String]): IO[DocumentState] =

    for
      content <- Ref.of[IO, String](content.getOrElse(""))
      result <- documents
        .modify: docs =>
          docs.get(uri) match
            case Some(existing) => docs -> IO(existing)
            case None =>
              val newDoc = new DocumentState(content)
              (docs + (uri -> newDoc)) -> IO(newDoc)
        .flatten
    yield result
