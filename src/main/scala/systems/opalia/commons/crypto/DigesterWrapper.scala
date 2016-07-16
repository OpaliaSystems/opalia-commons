package systems.opalia.commons.crypto


trait DigesterWrapper {

  def update(byte: Byte): Unit

  def update(bytes: Array[Byte]): Unit

  def update(bytes: Array[Byte], i: Int, i1: Int): Unit

  def doFinal(): Array[Byte]
}
