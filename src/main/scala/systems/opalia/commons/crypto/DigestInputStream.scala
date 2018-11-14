package systems.opalia.commons.crypto

import java.io.{FilterInputStream, InputStream}


abstract class DigestInputStream private[crypto](inputStream: InputStream)
  extends FilterInputStream(inputStream) {

  override def read(): Int = {

    val value = this.in.read()

    if (value != -1)
      update(value.asInstanceOf[Byte])

    value
  }

  override def read(bytes: Array[Byte]): Int = {

    val value = this.in.read(bytes)

    if (value != -1)
      update(bytes, 0, value)

    value
  }

  override def read(bytes: Array[Byte], offset: Int, length: Int): Int = {

    val value = this.in.read(bytes, offset, length)

    if (value != -1)
      update(bytes, offset, value)

    value
  }

  def readAll(onlyIfAvailable: Boolean = false): Int = {

    val buffer = new Array[Byte](4096)

    var count = 0
    var sum = 0

    do {

      if (onlyIfAvailable && this.in.available() == 0)
        count = -1
      else
        count = read(buffer)

      if (count != -1)
        sum += count

    } while (count != -1)

    sum
  }

  def update(byte: Byte): Unit

  def update(bytes: Array[Byte]): Unit

  def update(bytes: Array[Byte], offset: Int, length: Int): Unit

  def doFinal(): Array[Byte]

  override def toString: String =
    s"[Digester Input Stream] ${this.in}"
}
