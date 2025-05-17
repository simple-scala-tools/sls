package org.scala.abusers.sls

import langoustine.lsp.Invocation
import cats.effect.std.Mutex
import cats.effect.IO
import langoustine.lsp.structures.*
import java.net.URI
import org.scala.abusers.sls.NioConverter.asNio
import bsp.CompileParams
import langoustine.lsp.Communicate

object SyncManager {

  def instance(textDocumentSyncManager: TextDocumentSyncManager, bspStateManager: BspStateManager): IO[SyncManager] =
    Mutex[IO].map(SyncManager(textDocumentSyncManager, bspStateManager, _))

}

class SyncManager(textDocumentSyncManager: TextDocumentSyncManager, bspStateManager: BspStateManager, mutex: Mutex[IO]) {
  def didOpen(in: Invocation[DidOpenTextDocumentParams, IO]): IO[Unit] =
    mutex.lock.surround {
      textDocumentSyncManager.didOpen(in) *> bspStateManager.didOpen(in)
    }

  def didChange(in: Invocation[DidChangeTextDocumentParams, IO]) =
    mutex.lock.surround {
      textDocumentSyncManager.didChange(in)
    }

  def didClose(in: Invocation[DidCloseTextDocumentParams, IO]): IO[Unit] =
    mutex.lock.surround {
      textDocumentSyncManager.didClose(in)
    }

  def didSave(in: Invocation[DidSaveTextDocumentParams, IO]): IO[Unit] =
    mutex.lock.surround {
      for {
        _ <- textDocumentSyncManager.didSave(in)
        info <- bspStateManager.get(in.params.textDocument.uri.asNio)
      } yield info
    }.flatMap { info =>
      bspStateManager.bspServer.generic.buildTargetCompile(CompileParams(targets = List(info.buildTarget.id)))
    }.void

  def getDocumentState(uri: URI): IO[DocumentState] =
    mutex.lock.surround {
      textDocumentSyncManager.get(uri)
    }

  def getBuildTargetInformation(uri: URI): IO[ScalaBuildTargetInformation] =
    mutex.lock.surround {
      bspStateManager.get(uri)
    }

  def importBuild(client: Communicate[IO]): IO[Unit] =
    mutex.lock.surround {
      bspStateManager.importBuild(client)
    }

}
