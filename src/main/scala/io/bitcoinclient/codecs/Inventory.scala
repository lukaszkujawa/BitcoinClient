package io.bitcoinclient.codecs

import io.bitcoinclient.codecs.VarInt.varint
import scodec.{Attempt, Codec, DecodeResult, Decoder, SizeBound}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.{bytes, uint32L}

// https://github.com/bitcoin/bitcoin/blob/master/src/protocol.h

class Inventory extends Codec[List[ByteVector]] {

  val elDecoder = uint32L.flatMap( msgType => msgType match {
    // MSG_TX
    case 1 => bytes(32)

    // MSG_BLOCK
    case 2 => bytes(64)

    // MSG_FILTERED_BLOCK
    case 3 => bytes(64)

    // Throw error!!!
    case _ => bytes(16)
  })

  override def encode(value: List[ByteVector]): Attempt[BitVector] = ???

  override def sizeBound: SizeBound = SizeBound.unknown

  override def decode(bits: BitVector): Attempt[DecodeResult[List[ByteVector]]] =
    varint.flatMap( num => Decoder.decodeCollect[List, ByteVector](elDecoder, Some(num)) ).decode(bits)

}

object Inventory {
  def inventory = new Inventory()
}