package systems.opalia.commons.codec


object Hex
  extends Codec[Seq[Byte]] {

  def encode(data: Seq[Byte]): String =
    data.map("%02x" format _).mkString

  def decode(data: String): Option[Seq[Byte]] =
    if (isValid(data))
      Some(data.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte))
    else
      None

  def isValid(data: String): Boolean =
    data.matches( """^([0-9A-Fa-f]{2})+$""")
}
