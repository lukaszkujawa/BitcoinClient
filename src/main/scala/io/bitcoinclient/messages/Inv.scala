package io.bitcoinclient.messages

import io.bitcoinclient.codecs.Inventory.inventory
import io.bitcoinclient.codecs.InventoryItem.inventoryitem
import io.bitcoinclient.codecs.VarInt.varint
import scodec.bits.ByteVector
import scodec._
import scodec.codecs._
import scodec.bits._

case class InvItem(msg_id: Int, payload: ByteVector) {
  override def toString = payload.reverse.toString().toLowerCase
}

case class Inv(count: Int, tems: List[InvItem]) extends Message {
  def toEither: Either[Err, Array[Byte]] = Right(Array[Byte]())
}

object Inv {
  val commandName = "inv"

  implicit val codec: Codec[Inv] = {
    ("count" | varint ) ::
    ("items" | list[InvItem](inventoryitem) )
  }.as[Inv]
}


