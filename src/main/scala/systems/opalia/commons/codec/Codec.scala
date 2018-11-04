package systems.opalia.commons.codec


trait Codec {

  def encode(data: Seq[Byte]): String

  def encode(data: Seq[Byte], charset: String): String

  def encodeFromString(data: String): String

  def encodeFromString(data: String, charset: String): String

  def decode(data: String): IndexedSeq[Byte]

  def decode(data: String, charset: String): IndexedSeq[Byte]

  def decodeToString(data: String): String

  def decodeToString(data: String, charset: String): String

  def decodeOpt(data: String): Option[IndexedSeq[Byte]]

  def decodeOpt(data: String, charset: String): Option[IndexedSeq[Byte]]

  def decodeToStringOpt(data: String): Option[String]

  def decodeToStringOpt(data: String, charset: String): Option[String]

  def isValid(data: String): Boolean
}
