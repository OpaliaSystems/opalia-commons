package systems.opalia.commons.codec

import java.util.{Base64 => Base64Handler}
import scala.util.Try


object Base64
  extends Codec[Seq[Byte]] {

  def encode(data: Seq[Byte]): String =
    new String(Base64Handler.getEncoder.encode(data.toArray))

  def decode(data: String): Option[Seq[Byte]] =
    Try(Base64Handler.getDecoder.decode(data).toSeq).toOption

  def isValid(data: String): Boolean =
    data.matches("""^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{4}|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)$""")
}
