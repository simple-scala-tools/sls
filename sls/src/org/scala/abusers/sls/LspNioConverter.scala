package org.scala.abusers.sls

import langoustine.lsp.runtime.DocumentUri

import java.net.URI

object LspNioConverter {
  extension (documentUri: DocumentUri) def asNio: URI = URI.create(documentUri.value)
}
