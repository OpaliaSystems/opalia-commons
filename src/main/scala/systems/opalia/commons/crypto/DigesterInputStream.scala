package systems.opalia.commons.crypto

import java.io.{FilterInputStream, InputStream}


class DigesterInputStream(data: InputStream, digester: DigesterWrapper)
  extends FilterInputStream(data) {

  def getDigester: DigesterWrapper =
    digester

  override def read(): Int = {

    val value = this.in.read()

    if (value != -1)
      digester.update(value.asInstanceOf[Byte])

    value
  }

  override def read(bytes: Array[Byte], i: Int, i1: Int): Int = {

    val value = this.in.read(bytes, i, i1)

    if (value != -1)
      digester.update(bytes, i, value)

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

  override def toString: String = {
    "[Digester Input Stream] " + digester.toString
  }
}
