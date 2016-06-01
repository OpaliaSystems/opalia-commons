package systems.opalia.commons.codec

import org.apache.commons.codec.binary.{Base64 => Base64Handler}
import scala.util.Try


object Base64
  extends Codec[Seq[Byte]] {

  def encode(data: Seq[Byte]): String =
    Base64Handler.encodeBase64String(data.toArray)

  def decode(data: String): Option[Seq[Byte]] =
    Try(Base64Handler.decodeBase64(data).toSeq).toOption

  def isValid(data: String): Boolean =
    Base64Handler.isBase64(data)
}
