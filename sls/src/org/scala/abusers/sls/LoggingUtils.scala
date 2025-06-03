package org.scala.abusers.sls

import cats.effect.IO
import langoustine.lsp.*
import langoustine.lsp.structures.LogMessageParams
import langoustine.lsp.structures.ShowMessageParams
import langoustine.lsp.Communicate

object LoggingUtils {
  extension (back: Communicate[IO]) {
    def sendMessage(msg: String): IO[Unit] =
      back.notification(
        requests.window.showMessage,
        ShowMessageParams(enumerations.MessageType.Info, msg),
      ) *> logMessage(msg)

    def logMessage(message: String): IO[Unit] =
      back.notification(
        requests.window.logMessage,
        LogMessageParams(enumerations.MessageType.Info, message),
      )

    def logDebug(message: String): IO[Unit] =
      back.notification(
        requests.window.logMessage,
        LogMessageParams(enumerations.MessageType.Debug, message),
      )
  }
}
