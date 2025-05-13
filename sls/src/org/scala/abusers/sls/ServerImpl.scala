package org.scala.abusers.sls

import cats.effect.kernel.Ref
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
import org.scala.abusers.pc.ScalaVersion
import org.scala.abusers.sls.LspNioConverter.asNio

import java.net.URI
import scala.concurrent.duration.*

class ServerImpl(
    textDocumentSync: DocumentSyncManager,
    pcProvider: PresentationCompilerProvider,
    inverseSource: InverseSourcesToTarget,
    cancelTokens: IOCancelTokens,
):

  def handleCompletion(in: Invocation[CompletionParams, IO])(using stateRef: Ref[IO, State]) =
    val uri = in.params.textDocument.uri.asNio
    cancelTokens.mkCancelToken.use: token =>
        for
          state <- stateRef.get
          bloop = state.bspClient.get
          buildTargetsForFile <- inverseSource
            .get(uri)
            .map(_.toSet) // file may be owned by multiple targets eg. with crossbuild double check this
          allTargets <- bloop.generic.workspaceBuildTargets()
          // TODO: goto type definition with container types
          ourTarget        = allTargets.targets.collect { case b if buildTargetsForFile.contains(b.id) => b }
          scalaBuildTarget = ourTarget.headOption.getOrElse(sys.error("our target not found"))
          classpath <- bloop.scala.buildTargetScalacOptions(buildTargetsForFile.toList)
          scalaVersion = scalaBuildTarget.project.scala.get.data.get.scalaVersion
          pc <- pcProvider.get(
            ScalaVersion(
              scalaVersion
            ), // change to something good like BuildTargetIdentifier and manage its state in e.g BspState with Map[BuildTargetIdentifier, BuildTarget]
            classpath.items.flatMap(c => c.classpath).map(str => os.Path(URI(str))),
          )
          docState <- textDocumentSync.get(uri)
          cs       <- docState.getContent
          params = in.params.toOffsetParams(cs, token)
          result <- IO.fromCompletableFuture(IO(pc.complete(params)))
        yield Opt(convert[lsp4j.CompletionList, lngst.CompletionList](result))

  def handleDidSave(in: Invocation[DidSaveTextDocumentParams, IO])(using stateRef: Ref[IO, State]) =
    // for
    //   _     <- textDocumentSync.didSave(in)
    //   state <- stateRef.get
    //   bloop = state.bloopConn.get.client
    //   targets <- bloop.workspaceBuildTargets()
    //   targets0 = targets.targets
    //     // todo: dispatch to all the targets or wait for smithy4s to add mixins even without @adt
    //     .map(t => t.project.scala.getOrElse(sys.error(s"not a scala target: $t")))
    //     .map(_.id)
    //   // ourTarget <- targets.targets.find(in.params.textDocument.uri)
    //   buildTarget <- inverseSource.get(in.params.textDocument.uri.asNio)
    //   result <- bloop.buildTargetCompile(buildTarget) // straight to jar here ?? TODO add ID
    //   _                 <- logMessage(in.toClient, s"${result}")
    //   generatedByMetals <- logMessage(in.toClient, s"Build target: ${buildTarget}")
    // yield generatedByMetals
    for
      state <- stateRef.get
      bloop = state.bspClient.get
      _           <- textDocumentSync.didSave(in)
      buildTarget <- inverseSource.get(in.params.textDocument.uri.asNio)
      result      <- bloop.generic.buildTargetCompile(buildTarget) // straight to jar here ?? TODO add ID
    yield ()

  def handleDidOpen(in: Invocation[DidOpenTextDocumentParams, IO])(using stateRef: Ref[IO, State]) =
    for
      state <- stateRef.get
      bloop = state.bspClient.get
      _ <- textDocumentSync.didOpen(in)
      _ <- inverseSource.didOpen(bloop.generic, in)
    yield ()

  def handleDidClose(in: Invocation[DidCloseTextDocumentParams, IO])(using stateRef: Ref[IO, State]) =
    textDocumentSync.didClose(in)

  def handleDidChange(in: Invocation[DidChangeTextDocumentParams, IO])(using stateRef: Ref[IO, State]) =
    textDocumentSync.didChange(in)

  def handleInitialize(steward: ResourceSupervisor[IO])(in: Invocation[InitializeParams, IO])(using
      stateRef: Ref[IO, State]
  ) =
    val rootUri  = in.params.rootUri.toOption.getOrElse(sys.error("what now?"))
    val rootPath = os.Path(java.net.URI.create(rootUri.value).getPath())
    (for
      _         <- sendMessage(in.toClient, "ready to initialise!")
      _         <- importMillBsp(rootPath, in.toClient)
      bspClient <- connectWithBloop(in.toClient, steward)
      _         <- logMessage(in.toClient, "Connection with bloop estabilished")
      response <- bspClient.generic.buildInitialize(
        displayName = "bloop",
        version = "0.0.0",
        bspVersion = "2.1.0",
        rootUri = bsp.URI(rootUri.value),
        capabilities = bsp.BuildClientCapabilities(languageIds = List(bsp.LanguageId("scala"))),
      )
      _ <- logMessage(in.toClient, s"Response from bsp: $response")
      _ <- bspClient.generic.onBuildInitialized()
      _ <- stateRef.update(_.withBspClient(bspClient))
    yield InitializeResult(
      capabilities = serverCapabilities,
      serverInfo = Opt(InitializeResult.ServerInfo("My first LSP!")),
    )).guaranteeCase(s => logMessage(in.toClient, s"closing initalize with $s"))

  private def serverCapabilities: ServerCapabilities =
    ServerCapabilities(
      textDocumentSync = Opt(enumerations.TextDocumentSyncKind.Incremental),
      completionProvider = Opt(CompletionOptions(triggerCharacters = Opt(Vector(".")))),
    )

  private def connectWithBloop(back: Communicate[IO], steward: ResourceSupervisor[IO]): IO[BuildServer] =
    val temp       = os.temp.dir(prefix = "sls") // TODO Investigate possible clashes during reconnection
    val socketFile = temp / s"bloop.socket"
    val bspProcess = ProcessBuilder("bloop", "bsp", "--socket", socketFile.toNIO.toString())
      .spawn[IO]
      .flatMap { bspSocketProc =>
        bspSocketProc.stdout
          .merge(bspSocketProc.stderr)
          .through(text.utf8.decode)
          .through(text.lines)
          .evalMap(s => logMessage(back, s"[bloop] $s"))
          .onFinalizeCase(c => sendMessage(back, s"Bloop process terminated $c"))
          .compile
          .drain
          .background
      }
      .as(socketFile)

    val bspClientRes = for
      socketPath <- bspProcess
      _          <- Resource.eval(IO.sleep(1.seconds) *> logMessage(back, s"Looking for socket at $socketPath"))
      channel    <- FS2Channel.resource[IO]().flatMap(_.withEndpoints(bspClientHandler(back)))
      client     <- makeBspClient(socketPath.toString, channel, msg => logMessage(back, s"reportin raw: $msg"))
    yield client

    steward.acquire(bspClientRes)

  def importMillBsp(rootPath: os.Path, back: Communicate[IO]) =
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
