package org.scala.abusers.sls

import langoustine.lsp.Communicate
import cats.effect.IO
import langoustine.lsp.*
import langoustine.lsp.structures.ShowMessageParams
import langoustine.lsp.structures.LogMessageParams


object LoggingUtils {
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

  def logDebug(back: Communicate[IO], message: String): IO[Unit] =
    back.notification(
      requests.window.logMessage,
      LogMessageParams(enumerations.MessageType.Debug, message),
    )
}
