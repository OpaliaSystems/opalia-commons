package systems.opalia.commons.codec

import org.apache.commons.codec.binary.{Base64 => Base64Handler}
import scala.util.Try


object Base64
  extends Codec[Array[Byte]] {

  def encode(data: Array[Byte]): String =
    Base64Handler.encodeBase64String(data)

  def decode(data: String): Option[Array[Byte]] =
    Try(Base64Handler.decodeBase64(data)).toOption

  def isValid(data: String): Boolean =
    Base64Handler.isBase64(data)
}
