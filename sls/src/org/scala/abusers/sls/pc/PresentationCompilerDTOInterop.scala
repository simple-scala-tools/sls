package org.scala.abusers.pc

import langoustine.lsp.structures.CompletionParams
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler
import org.scala.abusers.sls.DocumentState
import upickle.default.*

import java.net.URI
import java.util.Collections
import scala.meta.pc.CancelToken
import scala.meta.pc.OffsetParams

object PresentationCompilerDTOInterop:
  val gson =
    MessageJsonHandler(Collections.emptyMap).getDefaultGsonBuilder().create()

  // We need to translate lsp4j / scalameta into langoustine and vice versa. It may be good idea to use chimney here

  // FIXME VVVVV Missing completion for langoustine
  def convert[In, Out: Reader](x: In): Out = read[Out](gson.toJson(x, x.getClass))

  extension (x: CompletionParams)
    def toOffsetParams(doc: DocumentState, cancelToken: CancelToken): OffsetParams =
      import doc.*
      new OffsetParams:
        override def toString(): String =
          s"""offset: $offset
             |$uri
             |$text""".stripMargin
        def offset(): Int        = x.position.toOffset
        def text(): String       = doc.content
        def token(): CancelToken = cancelToken
        def uri(): URI           = URI.create(x.textDocument.uri.value)
