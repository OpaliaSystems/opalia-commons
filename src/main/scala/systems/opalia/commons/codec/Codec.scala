package systems.opalia.commons.codec

import java.nio.charset.Charset


trait Codec {

  def encode(data: Seq[Byte]): String

  def encode(data: Seq[Byte], charset: Charset): String

  def encodeFromString(data: String): String

  def encodeFromString(data: String, charset: Charset): String

  def decode(data: String): IndexedSeq[Byte]

  def decode(data: String, charset: Charset): IndexedSeq[Byte]

  def decodeToString(data: String): String

  def decodeToString(data: String, charset: Charset): String

  def decodeOpt(data: String): Option[IndexedSeq[Byte]]

  def decodeOpt(data: String, charset: Charset): Option[IndexedSeq[Byte]]

  def decodeToStringOpt(data: String): Option[String]

  def decodeToStringOpt(data: String, charset: Charset): Option[String]

  def isValid(data: String): Boolean
}
