package systems.opalia.commons.codec

import java.util.{Base64 => Base64Handler}
import scala.util.Try
import systems.opalia.interfaces.rendering.Renderer


object Base64
  extends Codec[Seq[Byte]] {

  def encode(data: Seq[Byte]): String =
    encode(data, Renderer.defaultCharset)

  def encode(data: Seq[Byte], charset: String): String =
    new String(Base64Handler.getEncoder.encode(data.toArray), charset)

  def decode(data: String): Option[Seq[Byte]] =
    decode(data, Renderer.defaultCharset)

  def decode(data: String, charset: String): Option[Seq[Byte]] =
    Try(Base64Handler.getDecoder.decode(data.getBytes(charset)).toSeq).toOption

  def isValid(data: String): Boolean =
    data.matches("""^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{4}|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)$""")
}
