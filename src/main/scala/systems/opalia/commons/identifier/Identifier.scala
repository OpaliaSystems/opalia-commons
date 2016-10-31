package systems.opalia.commons.identifier

import java.util.Objects


trait Identifier
  extends IndexedSeq[Byte] {

  protected val id: Vector[Byte]

  override def apply(index: Int): Byte =
    id(index)

  override def length: Int =
    id.length

  override def equals(that: Any): Boolean =
    that match {

      case that: Identifier if (this.getClass == that.getClass && this.sameElements(that)) => true
      case _ => false
    }

  override def hashCode: Int =
    Objects.hash(id.map(Byte.box): _*)
}
