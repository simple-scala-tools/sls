package org.scala.abusers.sls

import bsp.CompileParams
import bsp.InitializeBuildParams
import cats.effect.kernel.Deferred
import cats.effect.kernel.Resource
import cats.effect.IO
import cats.syntax.all.*
import fs2.io.process.ProcessBuilder
import fs2.text
import jsonrpclib.fs2.FS2Channel
import langoustine.lsp.*
import langoustine.lsp.runtime.*
import langoustine.lsp.structures.*
import langoustine.lsp.structures as lngst
import org.eclipse.lsp4j
import org.scala.abusers.pc.IOCancelTokens
import org.scala.abusers.pc.PresentationCompilerDTOInterop.*
import org.scala.abusers.pc.PresentationCompilerProvider
import org.scala.abusers.sls.LspNioConverter.asNio

import java.util.concurrent.CompletableFuture
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.meta.pc.InlayHintsParams
import scala.meta.pc.OffsetParams
import scala.meta.pc.PresentationCompiler

class ServerImpl(
    textDocumentSync: DocumentSyncManager,
    pcProvider: PresentationCompilerProvider,
    bspStateManager: BspStateManager,
    cancelTokens: IOCancelTokens,
) {

  // // TODO: goto type definition with container types
  def handleCompletion(in: Invocation[CompletionParams, IO]) =
    offsetParamsRequest(in.params)(_.complete).map { result =>
      Opt(convert[lsp4j.CompletionList, lngst.CompletionList](result))
    }

  def handleHover(in: Invocation[HoverParams, IO]) =
    offsetParamsRequest(in.params)(_.hover).map { result =>
      Opt.fromOption(result.toScala.map(hoverSig => convert[lsp4j.Hover, lngst.Hover](hoverSig.toLsp())))
    }

  def handleSignatureHelp(in: Invocation[SignatureHelpParams, IO]) =
    offsetParamsRequest(in.params)(_.signatureHelp).map { result =>
      Opt(convert[lsp4j.SignatureHelp, lngst.SignatureHelp](result))
    }

  def handleDefinition(in: Invocation[DefinitionParams, IO]) =
    offsetParamsRequest(in.params)(_.definition).map { result =>
      Opt.fromOption(
        result
          .locations()
          .asScala
          .headOption
          .map(definition => convert[lsp4j.Location, aliases.Definition](definition))
      )
    }

  def handleInlayHints(in: Invocation[InlayHintParams, IO]) = {
    val uri0 = summon[WithURI[InlayHintParams]].uri(in.params)

    cancelTokens.mkCancelToken.use { token0 =>
      for {
        docState <- textDocumentSync.get(uri0)
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

  private def offsetParamsRequest[Params: PositionWithURI, Result](params: Params)(
      thunk: PresentationCompiler => OffsetParams => CompletableFuture[Result]
  ): IO[Result] = { // TODO Completion on context bound inserts []
    val uri      = summon[WithURI[Params]].uri(params)
    val position = summon[WithPosition[Params]].position(params)
    cancelTokens.mkCancelToken.use { token =>
      for {
        docState <- textDocumentSync.get(uri)
        offsetParams = toOffsetParams(position, docState, token)
        result <- pcParamsRequest(params, offsetParams)(thunk)
      } yield result
    }
  }

  private def pcParamsRequest[Params: WithURI, Result, PcParams](params: Params, pcParams: PcParams)(
      thunk: PresentationCompiler => PcParams => CompletableFuture[Result]
  ): IO[Result] = { // TODO Completion on context bound inserts []
    val uri = summon[WithURI[Params]].uri(params)
    for {
      info   <- bspStateManager.get(uri)
      pc     <- pcProvider.get(info)
      result <- IO.fromCompletableFuture(IO(thunk(pc)(pcParams)))
    } yield result
  }

  def handleDidSave(in: Invocation[DidSaveTextDocumentParams, IO]) =
    for {
      _    <- textDocumentSync.didSave(in)
      info <- bspStateManager.get(in.params.textDocument.uri.asNio)
      _ <- bspStateManager.bspServer.generic.buildTargetCompile(
        CompileParams(
          targets = List(info.buildTarget.id)
        )
      ) // straight to jar here ?? TODO add ID
    } yield ()

  def handleDidOpen(in: Invocation[DidOpenTextDocumentParams, IO]) =
    for {
      _ <- textDocumentSync.didOpen(in)
      _ <- bspStateManager.didOpen(in)
    } yield ()

  def handleDidClose(in: Invocation[DidCloseTextDocumentParams, IO]) =
    textDocumentSync.didClose(in)

  def handleDidChange(in: Invocation[DidChangeTextDocumentParams, IO]) =
    for _ <- textDocumentSync.didChange(in)
    yield ()

  def handleInitialize(steward: ResourceSupervisor[IO], bspClientDeferred: Deferred[IO, BuildServer])(
      in: Invocation[InitializeParams, IO]
  ) = {
    val rootUri  = in.params.rootUri.toOption.getOrElse(sys.error("what now?"))
    val rootPath = os.Path(java.net.URI.create(rootUri.value).getPath())
    (for {
      _         <- sendMessage(in.toClient, "ready to initialise!")
      _         <- importMillBsp(rootPath, in.toClient)
      bspClient <- connectWithBloop(in.toClient, steward)
      _         <- logMessage(in.toClient, "Connection with bloop estabilished")
      response <- bspClient.generic.buildInitialize(
        InitializeBuildParams(
          displayName = "bloop",
          version = "0.0.0",
          bspVersion = "2.1.0",
          rootUri = bsp.URI(rootUri.value),
          capabilities = bsp.BuildClientCapabilities(languageIds = List(bsp.LanguageId("scala"))),
        )
      )
      _ <- logMessage(in.toClient, s"Response from bsp: $response")
      _ <- bspClient.generic.onBuildInitialized()
      _ <- bspClientDeferred.complete(bspClient)
      _ <- bspStateManager.importBuild
    } yield InitializeResult(
      capabilities = serverCapabilities,
      serverInfo = Opt(InitializeResult.ServerInfo("My first LSP!")),
    )).guaranteeCase(s => logMessage(in.toClient, s"closing initalize with $s"))
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

  private def connectWithBloop(back: Communicate[IO], steward: ResourceSupervisor[IO]): IO[BuildServer] = {
    def bspProcess(socketFile: os.Path) = ProcessBuilder("bloop", "bsp", "--socket", socketFile.toNIO.toString())
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
            .evalMap(s => logMessage(back, s"[bloop] $s"))
            .onFinalizeCase(c => sendMessage(back, s"Bloop process terminated $c"))
            .compile
            .drain
            .background
          // wait for the started message before proceeding
            <* started.get.toResource
        }

      }
      .as(socketFile)

    val bspClientRes = for {
      temp <- IO(os.temp.dir(prefix = "sls")).toResource // TODO Investigate possible clashes during reconnection
      socketFile = temp / s"bloop.socket"
      socketPath <- bspProcess(socketFile)
      _          <- Resource.eval(IO.sleep(1.seconds) *> logMessage(back, s"Looking for socket at $socketPath"))
      channel <- FS2Channel
        .resource[IO]()
        .flatMap(_.withEndpoints(bspClientHandler(back)))
      client <- makeBspClient(socketPath.toString, channel, msg => logMessage(back, s"reportin raw: $msg"))
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
          .evalMap(s => logMessage(back, s))
          .compile
          .drain
      }
  }

  def sendMessage(back: Communicate[IO], msg: String): IO[Unit] =
    back.notification(
      requests.window.showMessage,
      ShowMessageParams(enumerations.MessageType.Info, msg),
    ) *> logMessage(back, msg)

  def logMessage(back: Communicate[IO], message: String): IO[Unit] =
    back.notification(
      requests.window.logMessage,
      LogMessageParams(enumerations.MessageType.Info, message),
    )
}
