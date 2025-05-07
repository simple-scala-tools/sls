package org.scala.abusers.pc

import scala.collection.mutable.ArrayBuffer
import org.scala.abusers.sls.Chars
import langoustine.lsp.structures.CompletionParams
import scala.meta.pc.OffsetParams
import scala.meta.pc.CancelToken
import java.util.concurrent.CompletionStage
import java.net.URI
import java.util.concurrent.CompletableFuture

object PresentationCompilerDTOInterop:

  // We need to translate lsp4j / scalameta into langoustine and vice versa. It may be good idea to use chimney here

  private def calculateLineIndicesFromContents(cs: String) = // TODO temp solution just to make completions work, to be refactored into DocumentSync
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


  extension (x: CompletionParams)
    def toOffsetParams(cs: String): OffsetParams = new OffsetParams:
      override def toString(): String =
        s"""offset: $offset
           |$uri
           |$text""".stripMargin
      def offset(): Int = calculateLineIndicesFromContents(cs)(x.position.line.value) + x.position.character.value
      def text(): String = cs
      def token(): CancelToken = new CancelToken: // FIXME very bad
        def checkCanceled(): Unit = ()
        def onCancel(): CompletionStage[java.lang.Boolean] = CompletableFuture.completedFuture(java.lang.Boolean.FALSE)
      def uri(): URI = URI.create(x.textDocument.uri.value)
