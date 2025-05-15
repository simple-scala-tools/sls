package org.scala.abusers.sls // TODO package completions are still here when they should not, also we should get whole package completion out of the box

import cats.effect.*
import cats.effect.std.MapRef
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

case class DocumentState(content: String):
  // We could have implemented incremental linesCache updating based on textEdits, but I believe this will not cause any performance bottlenecks.
  private lazy val linesCache =
    val buf = new ArrayBuffer[Int]
    buf += 0
    var i = 0
    while i < content.length do
      val isLineBreak =
        val ch = content(i)
        // don't identify the CR in CR LF as a line break, since LF will do.
        if ch == Chars.CR then i + 1 == content.length || content(i + 1) != Chars.LF
        else Chars.isLineBreakChar(ch)
      if isLineBreak then buf += i + 1
      i += 1
    buf += content.length // sentinel, so that findLine below works smoother
    buf.toArray

  /** Map line to offset of first character in line */
  def lineToOffset(index: Int): Int = linesCache(index)

  def processEdits(edits: Vector[TextDocumentContentChangeEvent]): DocumentState =
    edits.toList
      .foldLeft(this):
        case (_, incremental: TextDocumentContentChangeEvent.S0) => applyEdit(incremental)
        case (_, full: TextDocumentContentChangeEvent.S1)        => DocumentState(full.text)
        case _                                                   => sys.error("Illegal State Exception")

  private def applyEdit(edit: TextDocumentContentChangeEvent.S0): DocumentState =
    val Range(startPos, endPos) = edit.range
    val startOffset             = lineToOffset(startPos.line.value) + startPos.character.value
    val endOffset               = lineToOffset(endPos.line.value) + endPos.character.value
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
