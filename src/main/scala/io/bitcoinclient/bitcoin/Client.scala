package io.bitcoinclient.bitcoin

import cats.data.EitherT
import cats.effect._
import io.bitcoinclient.bitcoin.messages.Header.getMessageName

import java.io.{DataInputStream, DataOutputStream}
import java.net.{InetAddress, Socket}
import io.bitcoinclient.bitcoin.messages.{Addr, GetAddr, Header, Message, VerAck, Version}
import io.bitcoinclient.utils.Debug

case class Client(conn: Connection) extends Runnable {

  def run() = {
    while(true) {
      (for {
        rawResponseHeader <- EitherT(conn.readBytes(24))
        responseHeader = Header.fromByteArray(rawResponseHeader)
        payload <- EitherT(conn.readBytes(responseHeader.payloadSize))
        message = responseHeader.messageFromPayload(payload)
        _ = message match {
          case Right(a: Addr) => println(a)
          case _ => null
        }
        /*
        _ = println("---------------")
        _ = println("| " + responseHeader.commandName.filter(_>0).map(_.toChar).mkString)
        _ = println("---Header -----")
        _ = Debug.dumpBinary(rawResponseHeader)
        _ = println("\n--- Payload("+ responseHeader.payloadSize + ") -----")
        _ = Debug.dumpBinary(payload)
        _ = println("========\n")

         */
      } yield(payload)).value.unsafeRunSync()
    }
  }

  def listen(): IO[Unit] = IO(new Thread(this).start)


  def connect(): IO[Either[Unit, Header]] = {

    val versionMessage: Option[Message] = Version.create(
      70015,
      1,
      Array[Byte](0,0,0,0,0,0,0,0,0,0,0xFF.toByte,0xFF.toByte,0x7F,0,0,0x1),
      8333,
      "ABCDFEHJ",
      "/Satoshi:0.9.3/",
      665333)

    (for {
      ver <- EitherT.fromOption[IO](versionMessage, ())
      header = Header.create(ver)
      _ <- EitherT(conn.write(header.toArray))
      _ <- EitherT(conn.write(versionMessage.get.toArray))
      data <- EitherT(conn.readBytes(24))
      responseHeader = Header.fromByteArray(data)
      rawVersion <- EitherT(conn.readBytes(responseHeader.payloadSize))
      responseVersion = Version.fromByteArray(rawVersion)
      verAck = Header.create(VerAck())
      _ <- EitherT(conn.write(verAck.toArray))
      data2 <- EitherT(conn.readBytes(24))
      responseHeader2 <- EitherT.fromEither[IO](Header.verAckFromByteArray(data2))
    } yield ((responseHeader2))).value

  }

  def getAddresses(): IO[Either[Unit, Unit]] = (
      for {
        _ <- EitherT(conn.write(Header.create(GetAddr()).toArray))
      } yield ()
    ).value



}

case class Connection(sock: Socket, in: DataInputStream, out: DataOutputStream) {

  def close: IO[Unit] = Connection.close(this)

  def write(payload: Array[Byte]): IO[Either[Unit, Unit]] = Connection.write(this, payload)

  def readBytes(num: Int): IO[Either[Unit, Array[Byte]]] = read(Array.fill(num)(0x00))

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