package yedis

import zio.*
import zio.Console.*

import core.*, model.*, DataType.*, DataType.given

object Main extends ZIOAppDefault:  
  val run =
    val program =
      for response  <- ProtocolClient.tranceive[List, Int](List.Append("key2", "Patrik Andersson"))
          _         <- printLine(s"Received: $response")
          response2 <- ProtocolClient.tranceive[List, Int](List.Prepend("key2", "Sanna Jakobson"))
           _        <- printLine(s"Received: $response2")
      yield ()

    program.provide(
      ZLayer.succeed(ConnectionSpec("localhost", 6379)),
      ZLayer.fromZIO(Connection.make),
      ProtocolClient.make,
    )