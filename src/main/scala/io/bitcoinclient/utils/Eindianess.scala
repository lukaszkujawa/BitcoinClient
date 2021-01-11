package io.bitcoinclient.utils

import java.nio.{ByteBuffer, ByteOrder}

package object Eindianess {

  implicit class ToLittleEndianInt(val value: Int) extends AnyVal {
    def toLittleEndian: Array[Byte] = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(value).array()
  }

  implicit class ToLittleEndianShort(val value: Short) extends AnyVal {
    def toLittleEndian: Array[Byte] = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(value).array().take(2)
  }

  implicit class ToLBigEndianShort(val value: Short) extends AnyVal {
    def toBigEndian: Array[Byte] = ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putInt(value).array().take(2)
  }

  implicit class ToIntFromLittleEndian(val value: Array[Byte]) extends AnyVal {
    def toIntFromLittleEndian: Int = ByteBuffer.wrap(value).order(ByteOrder.LITTLE_ENDIAN).asIntBuffer().get
  }

  implicit class ToIntFromBigEndian(val value: Array[Byte]) extends AnyVal {
    def toIntFromBigEndian: Int = ByteBuffer.wrap(value).order(ByteOrder.BIG_ENDIAN).asIntBuffer().get
  }

  implicit class ToShortFromBigEndian(val value: Array[Byte]) extends AnyVal {
    def toShortFromBigEndian: Short = ByteBuffer.wrap(value).order(ByteOrder.BIG_ENDIAN).asShortBuffer().get
  }


}
