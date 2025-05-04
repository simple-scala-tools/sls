package org.scala.abusers.sls

import langoustine.lsp.aliases.*
import langoustine.lsp.runtime.*
import langoustine.lsp.structures.*
import org.scala.abusers.sls.LspNioConverter.asNio
import weaver.SimpleIOSuite

object TextDocumentSyncSuite extends SimpleIOSuite:

  def makeChange(
      startLine: Int,
      startChar: Int,
      endLine: Int,
      endChar: Int,
      text: String,
  ): TextDocumentContentChangeEvent =
    TextDocumentContentChangeEvent
      .S0(
        range = Range(
          start = Position(startLine, startChar),
          end = Position(endLine, endChar),
        ),
        text = text,
      )
      .asInstanceOf[TextDocumentContentChangeEvent]

  def open(uri: DocumentUri, text: String): DidOpenTextDocumentParams =
    DidOpenTextDocumentParams(
      TextDocumentItem(uri = uri, languageId = "scala", version = 0, text = text)
    )

  loggedTest("applies full document change") { log =>
    val uri    = DocumentUri("/home/Test.scala")
    val client = TestClient(log)
    for
      mgr <- DocumentSyncManager.create
      _   <- mgr.didOpen(client.input(open(uri, "Hello!")))

      _ <- mgr.didChange(
        client.input(
          DidChangeTextDocumentParams(
            VersionedTextDocumentIdentifier(version = 1, uri = uri),
            contentChanges = Vector(
              TextDocumentContentChangeEvent
                .S1(
                  text = "val x = 1\nval y = 2"
                )
                .asInstanceOf[TextDocumentContentChangeEvent]
            ),
          )
        )
      )

      doc     <- mgr.get(uri.asNio)
      content <- doc.get.getContent
    yield expect.eql(expected = "val x = 1\nval y = 2", found = content)

  }

  loggedTest("applies incremental document change at the beggining") { log =>
    val uri    = DocumentUri("/home/Test.scala")
    val client = TestClient(log)
    for
      mgr <- DocumentSyncManager.create
      _   <- mgr.didOpen(client.input(open(uri, "val z = 3")))

      _ <- mgr.didChange(
        client.input(
          DidChangeTextDocumentParams(
            VersionedTextDocumentIdentifier(version = 1, uri = uri),
            contentChanges = Vector(
              makeChange(startLine = 0, startChar = 0, endLine = 0, endChar = 0, text = "val x = 1\nval y = 2\n")
            ),
          )
        )
      )

      doc     <- mgr.get(uri.asNio)
      content <- doc.get.getContent
    yield expect.eql(expected = "val x = 1\nval y = 2\nval z = 3", found = content)

  }

  loggedTest("applies incremental document change at the end") { log =>
    val uri    = DocumentUri("/home/Test.scala")
    val client = TestClient(log)
    for
      mgr <- DocumentSyncManager.create
      _   <- mgr.didOpen(client.input(open(uri, "val x = 1\nval y = 2")))

      // full document replacement
      _ <- mgr.didChange(
        client.input(
          DidChangeTextDocumentParams(
            VersionedTextDocumentIdentifier(version = 1, uri = uri),
            contentChanges = Vector(makeChange(startLine = 1, startChar = 9, endLine = 1, endChar = 9, text = "\n")),
          )
        )
      )

      doc     <- mgr.get(uri.asNio)
      content <- doc.get.getContent
    yield expect.eql(expected = "val x = 1\nval y = 2\n", found = content)

  }

  loggedTest("applies incremental document change with selection") { log =>
    val uri    = DocumentUri("/home/Test.scala")
    val client = TestClient(log)
    for
      mgr <- DocumentSyncManager.create
      _   <- mgr.didOpen(client.input(open(uri, "val x = 1\nval y = 2\nval z = 3")))

      // full document replacement
      _ <- mgr.didChange(
        client.input(
          DidChangeTextDocumentParams(
            VersionedTextDocumentIdentifier(version = 1, uri = uri),
            contentChanges = Vector(makeChange(startLine = 1, startChar = 0, endLine = 1, endChar = 9, text = "p")),
          )
        )
      )

      doc     <- mgr.get(uri.asNio)
      content <- doc.get.getContent
    yield expect.eql(expected = "val x = 1\np\nval z = 3", found = content)

  }
