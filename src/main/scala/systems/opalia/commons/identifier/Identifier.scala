package systems.opalia.commons.identifier

import java.util.Objects


trait Identifier
  extends IndexedSeq[Byte] {

  protected val bytes: Vector[Byte]
  protected val string: String

  override def apply(index: Int): Byte =
    bytes(index)

  override def length: Int =
    bytes.length

  override def equals(that: Any): Boolean =
    that match {

      case that: Identifier if (this.getClass == that.getClass && this.sameElements(that)) => true
      case _ => false
    }

  override def toString: String =
    string

  override def hashCode: Int =
    Objects.hash(bytes.map(Byte.box): _*)
}
