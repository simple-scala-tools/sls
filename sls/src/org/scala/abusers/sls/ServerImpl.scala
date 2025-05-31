package org.scala.abusers.sls

import bsp.InitializeBuildParams
import cats.effect.kernel.Deferred
import cats.effect.kernel.Resource
import cats.effect.IO
import cats.syntax.all.*
import fs2.io.process.ProcessBuilder
import fs2.text
import ScalaBuildTargetInformation.*
import jsonrpclib.fs2.FS2Channel
import langoustine.lsp.*
import langoustine.lsp.runtime.*
import langoustine.lsp.structures.*
import langoustine.lsp.structures as lngst
import org.eclipse.lsp4j
import org.scala.abusers.pc.IOCancelTokens
import LoggingUtils.*
import org.scala.abusers.pc.PresentationCompilerDTOInterop.*
import org.scala.abusers.pc.PresentationCompilerProvider
import org.scala.abusers.sls.NioConverter.asNio

import java.net.URI
import java.util.concurrent.CompletableFuture
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.meta.pc.CancelToken
import scala.meta.pc.InlayHintsParams
import scala.meta.pc.OffsetParams
import scala.meta.pc.PresentationCompiler
import scala.meta.pc.VirtualFileParams
import org.scala.abusers.pc.ScalaVersion

class ServerImpl(
    stateManager: StateManager,
    pcProvider: PresentationCompilerProvider,
    cancelTokens: IOCancelTokens,
    diagnosticManager: DiagnosticManager,
) {

  // // TODO: goto type definition with container types
  def handleCompletion(in: Invocation[CompletionParams, IO]) =
    offsetParamsRequest(in)(_.complete).map { result =>
      Opt(convert[lsp4j.CompletionList, lngst.CompletionList](result))
    }

  def handleHover(in: Invocation[HoverParams, IO]) =
    offsetParamsRequest(in)(_.hover).map { result =>
      Opt.fromOption(result.toScala.map(hoverSig => convert[lsp4j.Hover, lngst.Hover](hoverSig.toLsp())))
    }

  def handleSignatureHelp(in: Invocation[SignatureHelpParams, IO]) =
    offsetParamsRequest(in)(_.signatureHelp).map { result =>
      Opt(convert[lsp4j.SignatureHelp, lngst.SignatureHelp](result))
    }

  def handleDefinition(in: Invocation[DefinitionParams, IO]) =
    offsetParamsRequest(in)(_.definition).map { result =>
      Opt.fromOption(
        result
          .locations()
          .asScala
          .headOption
          .map(definition => convert[lsp4j.Location, aliases.Definition](definition))
      )
    }

  def virtualFileParams(uri0: URI, content: String, token0: CancelToken): VirtualFileParams = new VirtualFileParams {
    override def text(): String       = content
    override def token(): CancelToken = token0
    override def uri(): URI           = uri0
  }

  def handleInlayHints(in: Invocation[InlayHintParams, IO]) = {
    val uri0 = summon[WithURI[InlayHintParams]].uri(in.params)

    cancelTokens.mkCancelToken.use { token0 =>
      for {
        docState <- stateManager.getDocumentState(uri0)
        inalyHintsParams = new InlayHintsParams {
          import docState.*
          def implicitConversions(): Boolean     = true
          def implicitParameters(): Boolean      = true
          def inferredTypes(): Boolean           = true
          def typeParameters(): Boolean          = true
          def offset(): Int                      = in.params.range.start.toOffset
          def endOffset(): Int                   = in.params.range.end.toOffset
          def text(): String                     = content
          def token(): scala.meta.pc.CancelToken = token0
          def uri(): java.net.URI                = uri0
        }

        result <- pcParamsRequest(in.params, inalyHintsParams)(_.inlayHints)
      } yield Opt(convert[java.util.List[lsp4j.InlayHint], Vector[InlayHint]](result))
    }
  }

  // def handleInlayHintsRefresh(in: Invocation[Unit, IO]) = IO.pure(null)

  private def offsetParamsRequest[Params: PositionWithURI, Result](in: Invocation[Params, IO])(
      thunk: PresentationCompiler => OffsetParams => CompletableFuture[Result]
  ): IO[Result] = { // TODO Completion on context bound inserts []
    val uri      = summon[WithURI[Params]].uri(in.params)
    val position = summon[WithPosition[Params]].position(in.params)
    cancelTokens.mkCancelToken.use { token =>
      for {
        docState <- stateManager.getDocumentState(uri)
        offsetParams = toOffsetParams(position, docState, token)
        result <- pcParamsRequest(in.params, offsetParams)(thunk)
      } yield result
    }
  }

  private def pcParamsRequest[Params: WithURI, Result, PcParams](params: Params, pcParams: PcParams)(
      thunk: PresentationCompiler => PcParams => CompletableFuture[Result]
  ): IO[Result] = { // TODO Completion on context bound inserts []
    val uri = summon[WithURI[Params]].uri(params)
    for {
      info   <- stateManager.getBuildTargetInformation(uri)
      pc     <- pcProvider.get(info)
      result <- IO.fromCompletableFuture(IO(thunk(pc)(pcParams)))
    } yield result
  }

  def handleDidSave(in: Invocation[DidSaveTextDocumentParams, IO]) = stateManager.didSave(in)

  def handleDidOpen(in: Invocation[DidOpenTextDocumentParams, IO]) = stateManager.didOpen(in)

  def handleDidClose(in: Invocation[DidCloseTextDocumentParams, IO]) = stateManager.didClose(in)

  val handleDidChange: Invocation[DidChangeTextDocumentParams, IO] => IO[Unit] = {
    val debounce = Debouncer(300.millis)

    def isSupported(info: ScalaBuildTargetInformation): Boolean = {
      import scala.math.Ordered.orderingToOrdered
      info.scalaVersion > ScalaVersion("3.7.2")
    }

    /**
     * We want to debounce compiler diagnostics as they are expensive to compute and we can't really cancel them
     * as they are triggered by notification and AFAIK, LSP cancellation only works for requests.
     */
    def pcDiagnostics(in: Invocation[DidChangeTextDocumentParams, IO], info: ScalaBuildTargetInformation, uri: URI): IO[Unit] =
      cancelTokens.mkCancelToken.use { token =>
        for {
          _ <- in.toClient.logDebug("Getting PresentationCompiler diagnostics")
          textDocument <- stateManager.getDocumentState(uri)
          pc           <- pcProvider.get(info)
          params = virtualFileParams(uri, textDocument.content, token)
          diags <- IO.fromCompletableFuture(IO(pc.didChange(params)))
          langoustineDiags = convert[java.util.List[lsp4j.Diagnostic], Vector[lngst.Diagnostic]](diags)
          _ <- diagnosticManager.didChange(in, langoustineDiags)
        } yield ()
    }

    in => for {
      _ <- stateManager.didChange(in)
      _ <- in.toClient.logDebug("Updated DocumentState")
      uri = in.params.textDocument.uri.asNio
      info         <- stateManager.getBuildTargetInformation(uri)
      _ <- if isSupported(info) then debounce.debounce(pcDiagnostics(in, info, uri)) else IO.unit
    } yield ()
  }

  def handleInitialized(in: Invocation[InitializedParams, IO]): IO[Unit] = stateManager.importBuild(in.toClient)

  def handleInitialize(
      steward: ResourceSupervisor[IO],
      bspClientDeferred: Deferred[IO, BuildServer],
  )(
      in: Invocation[InitializeParams, IO]
  ) = {
    val rootUri  = in.params.rootUri.toOption.getOrElse(sys.error("what now?"))
    val rootPath = os.Path(java.net.URI.create(rootUri.value).getPath())
    (for {
      _         <- in.toClient.sendMessage("Ready to initialise!")
      _         <- importMillBsp(rootPath, in.toClient)
      bspClient <- connectWithBloop(in.toClient, steward, diagnosticManager)
      _         <- in.toClient.logMessage("Connection with bloop estabilished")
      response <- bspClient.generic.buildInitialize(
        InitializeBuildParams(
          displayName = "bloop",
          version = "0.0.0",
          bspVersion = "2.1.0",
          rootUri = bsp.URI(rootUri.value),
          capabilities = bsp.BuildClientCapabilities(languageIds = List(bsp.LanguageId("scala"))),
        )
      )
      _ <- in.toClient.logMessage(s"Response from bsp: $response")
      _ <- bspClient.generic.onBuildInitialized()
      _ <- bspClientDeferred.complete(bspClient)
    } yield InitializeResult(
      capabilities = serverCapabilities,
      serverInfo = Opt(InitializeResult.ServerInfo("My first LSP!")),
    )).guaranteeCase(s => in.toClient.logMessage(s"closing initalize with $s"))
  }

  private def serverCapabilities: ServerCapabilities =
    ServerCapabilities(
      textDocumentSync = Opt(enumerations.TextDocumentSyncKind.Incremental),
      completionProvider = Opt(CompletionOptions(triggerCharacters = Opt(Vector(".")))),
      hoverProvider = Opt(HoverOptions()),
      signatureHelpProvider = Opt(SignatureHelpOptions(Opt(Vector("(", "[", "{")), Opt(Vector(",")))),
      definitionProvider = Opt(DefinitionOptions()),
      inlayHintProvider = Opt(InlayHintOptions(resolveProvider = Opt(false))),
    )

  private def connectWithBloop(
      back: Communicate[IO],
      steward: ResourceSupervisor[IO],
      diagnosticManager: DiagnosticManager,
  ): IO[BuildServer] = {
    val temp       = os.temp.dir(prefix = "sls") // TODO Investigate possible clashes during reconnection
    val socketFile = temp / s"bloop.socket"
    val bspProcess = ProcessBuilder("bloop", "bsp", "--socket", socketFile.toNIO.toString())
      .spawn[IO]
      .flatMap { bspSocketProc =>
        IO.deferred[Unit].toResource.flatMap { started =>
          val waitForStart: fs2.Pipe[IO, Byte, Nothing] =
            _.through(fs2.text.utf8.decode)
              .through(fs2.text.lines)
              .find(_.contains("The server is listening for incoming connections"))
              .foreach(_ => started.complete(()).void)
              .drain

          bspSocketProc.stdout
            .observe(waitForStart)
            .merge(bspSocketProc.stderr)
            .through(text.utf8.decode)
            .through(text.lines)
            .evalMap(s => back.logMessage(s"[bloop] $s"))
            .onFinalizeCase(c => back.sendMessage(s"Bloop process terminated $c"))
            .compile
            .drain
            .background
          // wait for the started message before proceeding
            <* started.get.toResource
        }

      }
      .as(socketFile)

    val bspClientRes = for {
      socketPath <- bspProcess
      _ <- Resource.eval(IO.sleep(1.seconds) *> back.logMessage(s"Looking for socket at $socketPath"))
      channel <- FS2Channel
        .resource[IO]()
        .flatMap(_.withEndpoints(bspClientHandler(back, diagnosticManager)))
      client <- makeBspClient(socketPath.toString, channel, msg => back.logDebug(s"reporting raw: $msg"))
    } yield client

    steward.acquire(bspClientRes)
  }

  def importMillBsp(rootPath: os.Path, back: Communicate[IO]) = {
    val millExec = "./mill" // TODO if mising then findMillExec()
    ProcessBuilder(millExec, "--import", "ivy:com.lihaoyi::mill-contrib-bloop:", "mill.contrib.bloop.Bloop/install")
      .withWorkingDirectory(fs2.io.file.Path.fromNioPath(rootPath.toNIO))
      .spawn[IO]
      .use { process =>
        val logStdout = process.stdout
        val logStderr = process.stderr

        val allOutput = logStdout
          .merge(logStderr)
          .through(text.utf8.decode)
          .through(text.lines)

        allOutput
          .evalMap(back.logMessage)
          .compile
          .drain
      }
  }
}
