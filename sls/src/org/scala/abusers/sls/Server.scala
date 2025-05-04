package examples.smithy.server

import cats.effect.*
import cats.effect.std.Console
import cats.effect.Concurrent
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.*
import fs2.io.net.Datagram
import fs2.io.net.Network
import fs2.io.net.Socket
import fs2.io.net.SocketGroup
import fs2.io.net.SocketOption
import fs2.text
import fs2.Stream
import jsonrpclib.fs2.*
import jsonrpclib.smithy4sinterop.ClientStub
import jsonrpclib.smithy4sinterop.ServerEndpoints
import jsonrpclib.CallId
import jsonrpclib.Endpoint
import test.* // smithy4s-generated package

def client(path: String) =
  val socket = Network[IO].connect(UnixSocketAddress(path))

object ServerMain extends IOApp.Simple:

  // Reserving a method for cancelation.
  val cancelEndpoint = CancelTemplate.make[CallId]("$/cancel", identity, identity)

  // Implementing the generated interface
  class ServerImpl(client: TestClient[IO]) extends TestServer[IO]:
    def greet(name: String): IO[GreetOutput] = IO.pure(GreetOutput(s"Server says: hello $name !"))

    def ping(ping: String): IO[Unit] = client.pong(s"Returned to sender: $ping")

  def printErr(s: String): IO[Unit] = IO.consoleForIO.errorln(s)

  def run: IO[Unit] =
    val run = for
      channel    <- FS2Channel[IO](cancelTemplate = Some(cancelEndpoint))
      testClient <- ClientStub.stream(TestClient, channel)
      _          <- channel.withEndpointsStream(ServerEndpoints(new ServerImpl(testClient)))
      _ <- fs2.Stream
        .eval(IO.never) // running the server forever
        .concurrently(stdin[IO](512).through(lsp.decodeMessages).through(channel.inputOrBounce))
        .concurrently(channel.output.through(lsp.encodeMessages).through(stdout[IO]))
    yield {}

    // Using errorln as stdout is used by the RPC channel
    printErr("Starting server") >> run.compile.drain.guarantee(printErr("Terminating server"))
