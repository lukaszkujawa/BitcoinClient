package io.bitcoinclient


import io.bitcoinclient.codecs.VarInt.varint
import io.bitcoinclient.example.barDeco
import io.bitcoinclient.messages.{Header, Version}
import io.bitcoinclient.utils.Debug
import scodec.Attempt.Successful
import scodec.SizeBound.unknown
import scodec.{Attempt, DecodeResult, Decoder}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.{bytes, uint32, uint8}
import scodec.bits._
import scodec._
import scodec.codecs._
import scodec.bits._
import cats.effect.{Fiber, IO}


object example extends App {

  import scodec.bits._
  import scodec.Codec
  import scodec.codecs.implicits._

  case class Point(x: Int, y: Int)

  case class Line(start: Point, end: Point)

  case class Arrangement(lines: Vector[Line])

  val arr = Arrangement(Vector(
    Line(Point(0, 0), Point(10, 10)),
    Line(Point(0, 10), Point(10, 0))))

  val arrBinary = Codec.encode(arr).require


  val pari = uint8 :: uint32

  object foo extends Decoder[Int] {
    def decode(b: BitVector): Attempt[DecodeResult[Int]] = Successful(DecodeResult(b.take(32).toInt(), b.drop(32)))
  }

  case class Bar(a: Long, b: Long)

  object Bar {

  }

  val hex = hex"F9BEB4D973656E646865616465727300000000005DF6E0E2".bits

  //  pari.as[Bar]
  // println(pari.decode(hex).require.value)

  case class HHeader(magicBytes: Unit, commandName: ByteVector, payloadSize: Long, checkSum: ByteVector)

  var barDeco = {
    uint32 :: uint32
  }.as[Bar]

  object HHeader {
    implicit val codec: Codec[HHeader] = {
      ("magicBytes" | constant(hex"F9BEB4D97")) ::
        ("commandName" | bytes(12)) :: //.xmap[String](ascii.decode(_).require.value, ascii) ::
        ("payloadSize" | uint32) ::
        ("checkSum" | bytes(4))
    }.as[HHeader]
  }

  case class Woot(i: Int)

  var deco: Decoder[Woot] = int32.map(Woot(_))

  println(deco.decode(hex"ffffffff".bits))

  Codec.decode[HHeader](hex).map(r => println(r))


  println(
    bytes(4)
      .emap(bv => ascii.decodeValue(bv.bits))
      .decode(hex"61626364".bits)
      .toEither)

  println(Codec[String].encode("Foo"))

  println(bytes(4).encode(ByteVector.apply("abcdx".getBytes)))

  println("--")
  constant(hex"61626364")
    .encode("abcd")
    .map(_.toByteArray)
    .toEither
    .map(Debug.dumpBinary)


  bytes(8)
    .exmap(
      bv => ascii.decodeValue(bv.bits),
      (s: String) => Attempt.successful(ByteVector.apply(s.getBytes)))
    .encode("abcd")
    .map(_.toByteArray)
    .toEither
    .map(Debug.dumpBinary)



  println("---")

  (uint32.dropRight(ignore(32)))
  .decode(hex"000000FFFFFFFFFF".bits)
  .toEither
  .map(println)

  uint8.flatMap( len => bytes(len).map(b => ascii.decode(b.bits).toString))
  .decode(hex"0261626364".bits)
  .toEither
  .map(println)


  bool(8)
    .decode(hex"02".bits)
    .toEither
    .map(println)

  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val ctx = IO.contextShift(global)

  val io = IO(println("Hello!"))
  val fiber: IO[Fiber[IO, Unit]] = io.start

  fiber.unsafeRunSync()
}