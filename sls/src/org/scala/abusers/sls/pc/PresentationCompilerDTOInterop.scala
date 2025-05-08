package org.scala.abusers.pc

import langoustine.lsp.structures.CompletionParams
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler
import org.scala.abusers.sls.Chars
import upickle.default.*

import java.net.URI
import java.util.Collections
import scala.collection.mutable.ArrayBuffer
import scala.meta.pc.CancelToken
import scala.meta.pc.OffsetParams

object PresentationCompilerDTOInterop:
  val gson =
    MessageJsonHandler(Collections.emptyMap).getDefaultGsonBuilder().create()

  // We need to translate lsp4j / scalameta into langoustine and vice versa. It may be good idea to use chimney here

  private def calculateLineIndicesFromContents(
      cs: String
  ) = // TODO temp solution just to make completions work, to be refactored into DocumentSync
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

    // FIXME VVVVV Missing completion for langoustine
  def convert[In, Out: Reader](x: In): Out = read[Out](gson.toJson(x, x.getClass))

  extension (x: CompletionParams)
    def toOffsetParams(
        cs: String,
        cancelToken: CancelToken,
    ): OffsetParams =
      new OffsetParams:
        override def toString(): String =
          s"""offset: $offset
             |$uri
             |$text""".stripMargin
        def offset(): Int  = calculateLineIndicesFromContents(cs)(x.position.line.value) + x.position.character.value
        def text(): String = cs

        def token(): CancelToken = cancelToken

        def uri(): URI = URI.create(x.textDocument.uri.value)
