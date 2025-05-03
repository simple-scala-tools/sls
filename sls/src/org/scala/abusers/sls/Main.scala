package org.scala.abusers.sls

import cats.effect.*
import fs2.io.process.ProcessBuilder
import fs2.io.process.Processes
import fs2.text
import jsonrpclib.fs2.*
import langoustine.lsp.*
import langoustine.lsp.all.*
import langoustine.lsp.app.*

object MyServer extends LangoustineApp.Simple:
  override def server =
    IO.ref(Set.empty[String]).map(myLSP)

  def myLSP(files: Ref[IO, Set[String]]) =
    LSPBuilder
      .create[IO]
      .handleRequest(initialize) { in =>
        val rootUri  = in.params.rootUri.toOption.getOrElse(sys.error("what now?"))
        val rootPath = os.Path(java.net.URI.create(rootUri.value).getPath())
        sendMessage(in.toClient, "ready to initialise!") *>
          importBsp(rootPath, in.toClient) *>
          IO {
            InitializeResult(
              capabilities = ServerCapabilities(textDocumentSync = Opt(TextDocumentSyncKind.Full)),
              serverInfo = Opt(InitializeResult.ServerInfo("My first LSP!")),
            )
          }
      }
      .handleNotification(textDocument.didOpen) { in =>
        val documentUri = in.params.textDocument.uri.value
        files.updateAndGet(_ + documentUri).map(_.size).flatMap { count =>
          sendMessage(in.toClient, s"In total, $count files registered!")
        }
      }

  def importBsp(rootPath: os.Path, back: Communicate[IO]) =
    val alreadyInstalled = os.exists(rootPath / ".bloop")
    if !alreadyInstalled then
      ProcessBuilder("./mill", "mill.bsp.BSP/install")
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
    else IO.unit

  def sendMessage(back: Communicate[IO], msg: String) =
    back.notification(
      window.showMessage,
      ShowMessageParams(MessageType.Info, msg),
    )

  def logMeesage(back: Communicate[IO], message: String): IO[Unit] =
    back.notification(
      window.logMessage,
      LogMessageParams(MessageType.Info, message),
    )
