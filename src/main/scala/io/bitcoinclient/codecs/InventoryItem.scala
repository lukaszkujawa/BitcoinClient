package io.bitcoinclient.codecs

import io.bitcoinclient.messages.InvItem
import scodec.Attempt.{Failure, Successful}
import scodec.Err.General
import scodec.bits.{BitVector, ByteOrdering}
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

class InventoryItem extends Codec[InvItem] {

  override def decode(bits: BitVector): Attempt[DecodeResult[InvItem]] = {

    val n = bits.take(8).toInt(false, ByteOrdering.LittleEndian)

    val payloadSize = n match {
      case 1 => 32*8
      case 2 => 64*8
      case 3 => 64*8
      case _ => -1
    }

    if( payloadSize > 0 ) {
      Successful(DecodeResult(InvItem(n, bits.drop(32).take(payloadSize).toByteVector), bits.drop(32+payloadSize)))
    }
    else {
      Failure(Err.apply("Unable to parse inventory type: " + n))
    }

  }

  override def encode(value: InvItem): Attempt[BitVector] = ???

  override def sizeBound: SizeBound = SizeBound.unknown

}

object InventoryItem {
  def inventoryitem = new InventoryItem()
}