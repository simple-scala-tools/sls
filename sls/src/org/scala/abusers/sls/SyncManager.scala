package org.scala.abusers.sls

import bsp.CompileParams
import cats.effect.std.Mutex
import cats.effect.IO
import langoustine.lsp.structures.*
import langoustine.lsp.Communicate
import langoustine.lsp.Invocation
import org.scala.abusers.sls.NioConverter.asNio

import java.net.URI

object SyncManager {

  def instance(textDocumentSyncManager: TextDocumentSyncManager, bspStateManager: BspStateManager): IO[SyncManager] =
    Mutex[IO].map(SyncManager(textDocumentSyncManager, bspStateManager, _))

}

/** Class created to synchronize all access to the state of bsp / textDocument Original problem was that by separating
  * both of them, didOpen was called in sequence which I believe could end up by some other request stealing the lock.
  *
  * Before we didn't synchronize the state modification, and inlayHint request was sent instantly and it accessed
  * [[bspStateManager.get]] that was not yet completed ([[ MapRef ]] does not lock on get)
  *
  * By unifying this in single manager, we can control what has to be synchronized.
  */
class SyncManager(
    textDocumentSyncManager: TextDocumentSyncManager,
    bspStateManager: BspStateManager,
    mutex: Mutex[IO],
) {
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
    mutex.lock
      .surround {
        for {
          _    <- textDocumentSyncManager.didSave(in)
          info <- bspStateManager.get(in.params.textDocument.uri.asNio)
        } yield info
      }
      .flatMap { info =>
        bspStateManager.bspServer.generic.buildTargetCompile(CompileParams(targets = List(info.buildTarget.id)))
      }
      .void

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
