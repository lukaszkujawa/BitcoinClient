package io.bitcoinclient.codecs

import scodec.Attempt.Successful
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import scodec.bits.{BitVector, ByteOrdering}

class VarInt extends Codec[Int] {
  override def encode(value: Int): Attempt[BitVector] = ???

  override def sizeBound: SizeBound = SizeBound(8, None)

  override def decode(bits: BitVector): Attempt[DecodeResult[Int]] = {
    val n = bits.take(8).toInt(false, ByteOrdering.LittleEndian)
    val res = n match {
      case 0xFF => DecodeResult(bits.drop(8).take(64).toInt(false, ByteOrdering.LittleEndian), bits.drop(72))
      case 0xFE => DecodeResult(bits.drop(8).take(32).toInt(false, ByteOrdering.LittleEndian), bits.drop(40))
      case 0xFD => DecodeResult(bits.drop(8).take(16).toInt(false, ByteOrdering.LittleEndian), bits.drop(24))
      case _ => DecodeResult(n, bits.drop(8))
    }
    Successful(res)
  }
}

object VarInt {
  def varint = new VarInt()
}
