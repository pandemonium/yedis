package yedis
package core

import zio.*

import java.nio.*
import java.nio.channels.*
import java.nio.charset.* 
import java.net.*

import model.*, model.given
import Protocol as P


case class ConnectionSpec(host: String, port: Int)

trait Connection:
  def send(packet: P.Value): Task[Unit]
  def receive: IO[ProtocolClient.Fault, P.Value]

object Connection:
  val MagicSize = 1_024

  val make: RIO[ConnectionSpec, Connection] =
    val send = ByteBuffer.allocateDirect(MagicSize)
    val recv = ByteBuffer.allocateDirect(MagicSize)

    def connect(spec: ConnectionSpec) = ZIO.attempt {
      val socket =
        SocketChannel.open()
                     .setOption(StandardSocketOptions.SO_KEEPALIVE, true)
                     .setOption(StandardSocketOptions.TCP_NODELAY, true)
      if !socket.connect(InetSocketAddress(spec.host, spec.port))
        then socket.finishConnect()

      socket
    }

    for spec   <- ZIO.service[ConnectionSpec]
        socket <- connect(spec)
    yield Live(send, recv, socket)

  class Live(send: ByteBuffer, recv: ByteBuffer, channel: SocketChannel) 
      extends Connection:
    def send(packet: P.Value): Task[Unit] = ZIO.attempt {
      val textualData = Show[P.Value].show(packet)
      val data        = textualData.getBytes(StandardCharsets.US_ASCII)

//      println(s"Sending: `${textualData.replace("\r\n", "\\r\\n")}`.")

      send.clear().put(data).flip()
      |> channel.write
    }.unit

    /* This is interesting. How does it handle a humongous amount of data? */
    /* Also: when's it done? */
    def receive: IO[ProtocolClient.Fault, P.Value] = ZIO.attempt {
      @annotation.tailrec
      def loop(data: Seq[String]): Seq[String] =
        channel.read(recv.clear()) match
          case 0 | -1 => data
          case read =>
            recv.flip()
            val buffer = Array.ofDim[Byte](recv.remaining())
            recv.get(buffer)

            /* Encoding/ schmencoding. */
            val data1 = data :+ String(buffer, 0, read)
            if read == MagicSize then loop(data1) else data1

      loop(Seq.empty).mkString
      |> Read[P.Value].parse
    }
    .some /* Yeah, well. No. */
    .mapError(_ => ProtocolClient.Fault.UnrecognizedData)


trait ProtocolClient:
  def tranceive[A, B](command: A)(using Encoder[A], Decoder[B]): IO[Option[ProtocolClient.Fault], B]

object ProtocolClient:
  enum Fault:
    case ErrorResponse(prefix: P.ErrorPrefix, message: String)
    case ExceptionOccured(t: Throwable)
    case UnrecognizedData

  given Show[Fault] with
    def show(x: Fault) = x match
      case Fault.ErrorResponse(p, m) => s"Protocol error: ${P.Value.Error(p, m).show}"
      case Fault.ExceptionOccured(t) => s"Exception occured: $t"
      case Fault.UnrecognizedData    => s"Unrecognized data."

  val make: URLayer[Connection, ProtocolClient] = ZLayer.fromFunction(Live(_))

  def tranceive[A, B](
    command: A
  )(using Encoder[A], Decoder[B]): ZIO[ProtocolClient, Option[ProtocolClient.Fault], B] =
    ZIO.serviceWithZIO[ProtocolClient](_.tranceive(command))

  private class Live(connection: Connection) extends ProtocolClient:
    def tranceive[A, B](command: A)(using Encoder[A], Decoder[B]): IO[Option[ProtocolClient.Fault], B] =
      val outbound = Encoder[A].encode(command)
      val io =
        /* These can probably be raced. */
        for _ <- connection.send(outbound).mapError(Fault.ExceptionOccured(_))
            r <- connection.receive
        yield r

      io.flatMap { case P.Value.Error(p, m) => 
                     ZIO.fail(Fault.ErrorResponse(p, m))
                   case otherwise           => 
                     ZIO.succeed(Decoder[B].decode(otherwise))
        }
        .some
