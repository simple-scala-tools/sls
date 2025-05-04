package org.scala.abusers.sls

import bsp.BuildClientCapabilities
import bsp.BuildServer
import bsp.LanguageId
import cats.data.OptionT
import cats.effect.*
import cats.syntax.all.*
import fs2.io.process.ProcessBuilder
import fs2.io.process.Processes
import fs2.text
import jsonrpclib.fs2.catsMonadic
import jsonrpclib.fs2.FS2Channel
import langoustine.lsp.*
import langoustine.lsp.all.*
import langoustine.lsp.app.*

import scala.concurrent.duration.*

object MyServer extends LangoustineApp.Simple:

  private case class State(files: Set[String], bloopConn: Option[BloopConnection]):
    def release: IO[Unit]                          = OptionT.fromOption[IO](bloopConn).semiflatTap(_.cancel).value.void
    def withBloopConnection(conn: BloopConnection) = copy(bloopConn = Some(conn))

  private case class BloopConnection(client: BuildServer[IO], cancel: IO[Unit])

  override def server =
    Resource
      .make(IO.ref(State(Set.empty, none)))(ref =>
        ref
          .getAndUpdate(_.copy(bloopConn = None))
          .map(_.release)
      )
      .use(myLSP.andThen(IO(_)))
      .guaranteeCase(s => IO.consoleForIO.errorln(s"closing with $s"))

  private def myLSP(stateRef: Ref[IO, State]): LSPBuilder[IO] =
    LSPBuilder
      .create[IO]
      .handleRequest(initialize) { in =>
        val rootUri  = in.params.rootUri.toOption.getOrElse(sys.error("what now?"))
        val rootPath = os.Path(java.net.URI.create(rootUri.value).getPath())
        (for
          _         <- sendMessage(in.toClient, "ready to initialise!")
          _         <- importMillBsp(rootPath, in.toClient)
          bloopConn <- connectWithBloop(in.toClient)
          _         <- logMeesage(in.toClient, "Connection with bloop estabilished")
          response <- bloopConn.client.buildInitialize(
            displayName = "dupa",
            version = "0.0.0",
            bspVersion = "2.1.0",
            rootUri = bsp.URI("file:///home/kghost/workspace/sst/playground"),
            capabilities = BuildClientCapabilities(languageIds = List(LanguageId("scala"))),
          )
          _ <- logMeesage(in.toClient, s"Response from bsp: $response")
          _ <- bloopConn.client.onBuildInitialized()
          _ <- stateRef.update(_.withBloopConnection(bloopConn))
        yield InitializeResult(
          capabilities = ServerCapabilities(textDocumentSync = Opt(TextDocumentSyncKind.Full)),
          serverInfo = Opt(InitializeResult.ServerInfo("My first LSP!")),
        )).guaranteeCase(s => logMeesage(in.toClient, s"closing initalize with $s"))
      }
      .handleNotification(textDocument.didOpen) { in =>
        val documentUri = in.params.textDocument.uri.value
        stateRef
          .updateAndGet(state => state.copy(files = state.files + documentUri))
          .map(_.files.size)
          .flatMap { count =>
            sendMessage(in.toClient, s"In total, $count files registered!")
          }
          .guaranteeCase(s => logMeesage(in.toClient, s"closing notify with $s"))
      }

  private def connectWithBloop(back: Communicate[IO]): IO[BloopConnection] =
    val temp       = os.temp.dir(prefix = "sls") // TODO Investigate possible clashes during reconnection
    val socketFile = temp / s"bloop.socket"
    val bspProcess = ProcessBuilder("bloop", "bsp", "--socket", socketFile.toNIO.toString())
      .spawn[IO]
      .flatMap { bspSocketProc =>
        bspSocketProc.stdout
          .merge(bspSocketProc.stderr)
          .through(text.utf8.decode)
          .through(text.lines)
          .evalMap(s => logMeesage(back, s"[bloop] $s"))
          .onFinalizeCase(c => sendMessage(back, s"Bloop process terminated $c"))
          .compile
          .drain
          .background
      }
      .as(socketFile)

    (for
      socketPath <- bspProcess
      _          <- Resource.eval(IO.sleep(1.seconds) *> logMeesage(back, s"Looking for socket at $socketPath"))
      channel    <- FS2Channel.resource[IO]()
      client     <- makeBspClient(socketPath.toString, channel, msg => logMeesage(back, msg))
    yield client).allocated
      .map { case (client, cancel) => BloopConnection(client, cancel) }

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
        .evalMap(s => logMeesage(back, s))
        .compile
        .drain
    }

def sendMessage(back: Communicate[IO], msg: String): IO[Unit] =
  back.notification(
    window.showMessage,
    ShowMessageParams(MessageType.Info, msg),
  ) *> logMeesage(back, msg)

def logMeesage(back: Communicate[IO], message: String): IO[Unit] =
  back.notification(
    window.logMessage,
    LogMessageParams(MessageType.Info, message),
  )
