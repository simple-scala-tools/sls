package org.scala.abusers.pc

import langoustine.lsp.structures.*
import langoustine.lsp.structures.HoverParams
import langoustine.lsp.structures.Position
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler
import org.scala.abusers.sls.DocumentState
import org.scala.abusers.sls.NioConverter.asNio
import upickle.default.*

import java.net.URI
import java.util.Collections
import scala.meta.pc.CancelToken
import scala.meta.pc.OffsetParams

object PresentationCompilerDTOInterop {
  val gson =
    MessageJsonHandler(Collections.emptyMap).getDefaultGsonBuilder().create()

  // We need to translate lsp4j / scalameta into langoustine and vice versa. It may be good idea to use chimney here

  // FIXME VVVVV Missing completion for langoustine
  def convert[In, Out: Reader](x: In): Out = read[Out](gson.toJson(x, x.getClass))

  def toOffsetParams(position: Position, doc: DocumentState, cancelToken: CancelToken): OffsetParams = {
    import doc.*
    new OffsetParams {
      override def toString(): String =
        s"""offset: $offset
           |$uri
           |$text""".stripMargin
      def offset(): Int        = position.toOffset
      def text(): String       = doc.content
      def token(): CancelToken = cancelToken
      def uri(): URI           = doc.uri
    }
  }

  trait WithPosition[A] {
    def position(params: A): Position
  }

  trait WithRange[A] {
    def range(params: A): Range
  }

  trait WithURI[A] {
    def uri(params: A): URI
  }

  trait PositionWithURI[A] extends WithPosition[A] with WithURI[A]
  trait RangeWithURI[A]    extends WithRange[A] with WithURI[A]

  given PositionWithURI[CompletionParams] with {
    def position(params: CompletionParams): Position = params.position
    def uri(params: CompletionParams): URI           = params.textDocument.uri.asNio
  }

  given PositionWithURI[HoverParams] with { // TODO can't rename inside the type param
    def position(params: HoverParams): Position = params.position
    def uri(params: HoverParams): URI           = params.textDocument.uri.asNio
  }

  given PositionWithURI[SignatureHelpParams] with {
    def position(params: SignatureHelpParams): Position = params.position
    def uri(params: SignatureHelpParams): URI           = params.textDocument.uri.asNio
  }

  given PositionWithURI[DefinitionParams] with {
    def position(params: DefinitionParams): Position = params.position
    def uri(params: DefinitionParams): URI           = params.textDocument.uri.asNio
  }

  given RangeWithURI[InlayHintParams] with {
    def range(params: InlayHintParams): Range = params.range
    def uri(params: InlayHintParams): URI     = params.textDocument.uri.asNio
  }
}
