package systems.opalia.commons.codec


trait Codec[T] {

  def encode(data: T): String

  def decode(data: String): Option[T]

  def isValid(data: String): Boolean
}
