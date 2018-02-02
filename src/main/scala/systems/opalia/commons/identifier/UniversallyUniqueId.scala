package systems.opalia.commons.identifier

import java.nio.ByteBuffer
import java.util.UUID
import scala.util.Try
import systems.opalia.commons.application.SystemProperty
import systems.opalia.commons.codec.Hex


class UniversallyUniqueId private(protected val bytes: Vector[Byte])
  extends Identifier {

  protected val string =
    Hex.encode(bytes.slice(0, 4)) + "-" +
      Hex.encode(bytes.slice(4, 6)) + "-" +
      Hex.encode(bytes.slice(6, 8)) + "-" +
      Hex.encode(bytes.slice(8, 10)) + "-" +
      Hex.encode(bytes.slice(10, 16))
}

object UniversallyUniqueId
  extends IdentifierCompanion[UniversallyUniqueId] {

  def isValid(that: String): Boolean = {

    val parts =
      that.split("-")

    if (parts.length != 5)
      false
    else
      parts
        .zip(List(4, 2, 2, 2, 6))
        .forall(part => Hex.isValid(part._1) && part._1.length == part._2 * 2)
  }

  def isValid(that: Seq[Byte]): Boolean =
    that.length == length

  def length: Int =
    16

  def getNew: UniversallyUniqueId = {

    val x =
      UUID.randomUUID()

    val bytes =
      ByteBuffer.allocate(length)
        .putLong(x.getMostSignificantBits) // 8 bytes
        .putLong(x.getLeastSignificantBits) // 8 bytes

    new UniversallyUniqueId(bytes.array.toVector)
  }

  def getFrom(that: String): UniversallyUniqueId =
    getFromOpt(that)
      .getOrElse(throw new IllegalArgumentException(s"Cannot generate UUID from: $that"))

  def getFrom(that: Seq[Byte]): UniversallyUniqueId =
    getFromOpt(that)
      .getOrElse(throw new IllegalArgumentException(s"Cannot generate UUID from: $that"))

  def getFromOpt(that: String): Option[UniversallyUniqueId] =
    if (UniversallyUniqueId.isValid(that))
      Try(UUID.fromString(that)).toOption.map((x) => {

        val bytes =
          ByteBuffer.allocate(length)
            .putLong(x.getMostSignificantBits) // 8 bytes
            .putLong(x.getLeastSignificantBits) // 8 bytes

        new UniversallyUniqueId(bytes.array.toVector)
      })
    else
      None

  def getFromOpt(that: Seq[Byte]): Option[UniversallyUniqueId] =
    if (UniversallyUniqueId.isValid(that))
      Some(new UniversallyUniqueId(that.toVector))
    else
      None

  def getFromName(that: String): UniversallyUniqueId =
    getFromName(that.getBytes(SystemProperty.defaultCharset))

  def getFromName(that: Seq[Byte]): UniversallyUniqueId = {

    val x =
      UUID.nameUUIDFromBytes(that.toArray)

    val bytes =
      ByteBuffer.allocate(length)
        .putLong(x.getMostSignificantBits) // 8 bytes
        .putLong(x.getLeastSignificantBits) // 8 bytes

    new UniversallyUniqueId(bytes.array.toVector)
  }
}
