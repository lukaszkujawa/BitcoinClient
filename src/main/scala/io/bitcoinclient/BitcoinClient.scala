package io.bitcoinclient

import cats.data.EitherT
import cats.effect.IO
import io.bitcoinclient.bitcoin.messages.Message
import io.bitcoinclient.messages.{GetAddr, Header, Mempool, Message, VerAck, Version}
import io.bitcoinclient.utils.Debug
import scodec.{Codec, Err}
import scodec.bits.{BitVector, ByteVector, HexStringSyntax}

import java.io.{DataInputStream, DataOutputStream}
import java.net.{InetAddress, Socket}

case class BitcoinClient(conn: Connection) extends Runnable
{

  def run() = {
    while(true) {
      val resp = (for {
        respRawHeader <- EitherT(conn.readBytes(Header.SIZE))
        respHeader <- EitherT.fromEither[IO](Header.fromByteArray(respRawHeader))
        payload <- EitherT(conn.readBytes(respHeader.payloadSize.toInt))
        _ = Message.parseMessage(respHeader, payload)
      } yield (respHeader, payload) ).value.unsafeRunSync()

      //println(resp.)
    }
  }

  def listen(): IO[Unit] = IO(new Thread(this).start)

  def getAddr() : IO[Either[Err,Unit]] = (for{
    header <-  EitherT.fromEither[IO](Header((), GetAddr.commandName, 0, Header.checksum(Array())).toEither)
    _ = conn.write(header)
  } yield()).value

  def mempool: IO[Either[Err, Unit]] = {
    val header = Header((), Mempool.commandName, 0, Header.checksum(Array()))

    (for {
      binHeader <- EitherT.fromEither[IO](header.toEither)
      _ = conn.write(binHeader)
    } yield ()).value
  }

  def connect = {

    val version = Version(
      70015,
      0,
      (System.currentTimeMillis / 1000).toInt,
      0, hex"00000000000000000000FFFF7F000001", 8333,
      0, hex"00000000000000000000FFFF7F000001", 8333,
      hex"AABBCCDD11223344",
      "/Satoshi:0.9.3/",
      665333,
      true
    )

    (for {
      // write header + version
      binVersion <- EitherT.fromEither[IO](version.toEither)
      header = Header((), Version.commandName, binVersion.length, Header.checksum(binVersion))
      binHeader <- EitherT.fromEither[IO](header.toEither)
      _ <- EitherT(conn.write(binHeader))
      _ <- EitherT(conn.write(binVersion))

      // read header + version
      respRawHeader <- EitherT(conn.readBytes(Header.SIZE))
      respHeader <- EitherT.fromEither[IO](Header.fromByteArray(respRawHeader))
      respRawVersion <- EitherT(conn.readBytes(respHeader.payloadSize.toInt))
      version <- EitherT.fromEither[IO](Version.fromByteArray(respRawVersion))

      // write header + verack
      header2 <- EitherT.fromEither[IO](Header((), VerAck.commandName, 0, Header.checksum(Array())).toEither)
      _ <- EitherT(conn.write(header2))

      // read header
      respRawHeader2 <- EitherT(conn.readBytes(Header.SIZE))
      respHeader2 <- EitherT.fromEither[IO](Header.fromByteArray(respRawHeader2))
    } yield(respHeader2)).value

  }



}


case class Connection(sock: Socket, in: DataInputStream, out: DataOutputStream) {

  def close: IO[Unit] = Connection.close(this)

  def write(payload: Array[Byte]): IO[Either[Unit, Unit]] = Connection.write(this, payload)

  def readBytes(num: Int): IO[Either[Any, Array[Byte]]] = read(Array.fill(num)(0x00))

  def read(buff: Array[Byte]): IO[Either[Unit, Array[Byte]]] = Connection.read(this, buff)

}

object Connection {

  def create(addr: String, port: Int): IO[Connection] =
    IO.pure(createFromSocket(
      new Socket(InetAddress.getByName(addr), port)))

  def createFromSocket(sock: Socket): Connection =
    Connection( sock,
      new DataInputStream(sock.getInputStream()),
      new DataOutputStream(sock.getOutputStream()))

  def close(conn: Connection): IO[Unit] = IO(conn.sock.close())

  def write(conn: Connection, payload: Array[Byte]): IO[Either[Unit, Unit]] = IO(
    try{
      Right(conn.out.write(payload))  }
    catch {
      case _ => Left(())
    })

  def read(conn: Connection, buff: Array[Byte]): IO[Either[Unit, Array[Byte]]] = IO(
    try {
      conn.in.readFully(buff)
      Right(buff)
    } catch {
      case _ => Left()
    }
  )

}