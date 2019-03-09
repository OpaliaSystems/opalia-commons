package systems.opalia.commons.codec

import java.nio.charset.Charset
import java.util.{Base64 => Base64Handler}
import scala.util.Try
import systems.opalia.interfaces.rendering.Renderer


object Base64
  extends Codec {

  def encode(data: Seq[Byte]): String =
    encode(data, Renderer.appDefaultCharset)

  def encode(data: Seq[Byte], charset: Charset): String =
    new String(Base64Handler.getEncoder.encode(data.toArray), charset)

  def encodeFromString(data: String): String =
    encodeFromString(data, Renderer.appDefaultCharset)

  def encodeFromString(data: String, charset: Charset): String =
    encode(data.getBytes(charset), charset)

  def decode(data: String): IndexedSeq[Byte] =
    decode(data, Renderer.appDefaultCharset)

  def decode(data: String, charset: Charset): IndexedSeq[Byte] =
    Base64Handler.getDecoder.decode(data.getBytes(charset)).toIndexedSeq

  def decodeToString(data: String): String =
    decodeToString(data, Renderer.appDefaultCharset)

  def decodeToString(data: String, charset: Charset): String =
    new String(decode(data, charset).toArray, charset)

  def decodeOpt(data: String): Option[IndexedSeq[Byte]] =
    decodeOpt(data, Renderer.appDefaultCharset)

  def decodeOpt(data: String, charset: Charset): Option[IndexedSeq[Byte]] =
    Try(decode(data, charset)).toOption

  def decodeToStringOpt(data: String): Option[String] =
    decodeToStringOpt(data, Renderer.appDefaultCharset)

  def decodeToStringOpt(data: String, charset: Charset): Option[String] =
    decodeOpt(data, charset).map(x => new String(x.toArray, charset))

  def isValid(data: String): Boolean =
    data.matches("""^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{4}|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)$""")
}
