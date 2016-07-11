package systems.opalia.commons.misc

import java.net.NetworkInterface
import java.nio.ByteBuffer
import java.util.Objects
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConversions._
import scala.util.Random
import systems.opalia.commons.application.SystemProperty
import systems.opalia.commons.codec.Hex


class ObjectId private(id: Vector[Byte])
  extends IndexedSeq[Byte] {

  override def apply(idx: Int): Byte =
    id(idx)

  override def length: Int =
    id.length

  override def equals(that: Any): Boolean =
    that match {

      case (that: ObjectId) if (this sameElements that) => true
      case _ => false
    }

  override def hashCode: Int =
    Objects.hash(id.map(Byte.box): _*)

  override def toString: String =
    Hex.encode(id)
}

object ObjectId {

  private object Generator {

    val counter = new AtomicInteger(Random.nextInt())

    def machinePart: Int = {

      val hardwareAddresses =
        NetworkInterface.getNetworkInterfaces.toSeq
          .map(_.getHardwareAddress)
          .filter(_ != null)
          .flatten

      Objects.hash(hardwareAddresses.map(Byte.box): _*)
    }

    def processPart: Int = {

      val processId = Int.box(SystemProperty.Process.pid)
      val classLoaderId = Int.box(System.identityHashCode(this.getClass.getClassLoader))

      Objects.hash(processId, classLoaderId)
    }

    def counterPart: Int = {

      counter.getAndIncrement
    }

    def timestampPart: Long = {

      System.currentTimeMillis()
    }

    def randomPart: Int = {

      Random.nextInt()
    }
  }

  def isValid(x: String): Boolean =
    Hex.isValid(x) && x.length == length * 2

  def isValid(x: Seq[Byte]): Boolean =
    x.length == length

  def length: Int =
    24

  def getNew: ObjectId = {

    val bytes =
      ByteBuffer.allocate(length)
        .putInt(Generator.machinePart) // 4 bytes
        .putInt(Generator.processPart) // 4 bytes
        .putInt(Generator.counterPart) // 4 bytes
        .putLong(Generator.timestampPart) // 8 bytes
        .putInt(Generator.randomPart) // 4 bytes

    new ObjectId(bytes.array.toVector)
  }

  def getFrom(that: String): Option[ObjectId] =
    if (ObjectId.isValid(that))
      Hex.decode(that).map((x) => new ObjectId(x.toVector))
    else
      None

  def getFrom(that: Seq[Byte]): Option[ObjectId] =
    if (ObjectId.isValid(that))
      Some(new ObjectId(that.toVector))
    else
      None
}
