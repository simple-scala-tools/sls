package org.scala.abusers.sls // TODO package completions are still here when they should not, also we should get whole package completion out of the box

import bsp.BuildTargetIdentifier
import cats.effect.*
import langoustine.lsp.structures.*
import langoustine.lsp.Invocation

import java.net.URI

object InverseSourcesToTarget:
  def instance: IO[InverseSourcesToTarget] =
    Ref.of[IO, Map[URI, List[BuildTargetIdentifier]]](Map.empty).map(InverseSourcesToTarget.apply)

/* It will be called for every request, and we can easily track those in cache */
class InverseSourcesToTarget(val sourceToTarget: Ref[IO, Map[URI, List[BuildTargetIdentifier]]]):

  private def findTarget(bspServer: bsp.BuildServer[IO], uri: URI): IO[List[BuildTargetIdentifier]] =
    bspServer
      .buildTargetInverseSources(bsp.TextDocumentIdentifier(bsp.URI(uri.toString)))
      .map(_.targets)

  def didOpen(bloop: bsp.BuildServer[IO], in: Invocation[DidOpenTextDocumentParams, IO]): IO[Unit] =
    val uri = URI.create(in.params.textDocument.uri.value)
    findTarget(bloop, uri).flatMap: targets =>
        sourceToTarget.update(_ + (uri -> targets))

  // didRename
  // didRemove

  def get(uri: URI): IO[List[BuildTargetIdentifier]] =
    sourceToTarget.get.flatMap(docs =>
      IO.fromOption(docs.get(uri))(IllegalStateException())
    ) // maybe we should not fail but fetch it ? Not sure how it will end up
