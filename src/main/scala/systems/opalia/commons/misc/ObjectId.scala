package systems.opalia.commons.misc

import java.lang.management.ManagementFactory
import java.net.NetworkInterface
import java.nio.ByteBuffer
import java.util.Objects
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConversions._
import scala.util.Random
import systems.opalia.commons.codec.Hex


class ObjectId private(id: Array[Byte])
  extends Serializable {

  override def equals(that: Any): Boolean =
    that match {

      case (that: ObjectId) if (this.toByteArray sameElements that.toByteArray) => true
      case _ => false
    }

  override def hashCode: Int =
    Objects.hash(id.map(Byte.box): _*)

  def toByteArray: Array[Byte] =
    id

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

      val pid = ManagementFactory.getRuntimeMXBean.getName.takeWhile(_ != '@').hashCode
      val hash = System.identityHashCode(this.getClass.getClassLoader)

      31 * hash + pid
    }

    def counterPart: Int = {

      counter.getAndIncrement
    }

    def timestampPart: Long = {

      System.currentTimeMillis()
    }

    def randomPart: Long = {

      Random.nextLong()
    }
  }

  def isValid(x: String): Boolean =
    Hex.isValid(x) && x.length == length * 2

  def isValid(x: Array[Byte]): Boolean =
    x.length == length

  def length: Int =
    28

  def getNew: ObjectId = {

    val bytes =
      ByteBuffer.allocate(length)
        .putInt(Generator.machinePart) // 4 bytes
        .putInt(Generator.processPart) // 4 bytes
        .putInt(Generator.counterPart) // 4 bytes
        .putLong(Generator.timestampPart) // 8 bytes
        .putLong(Generator.randomPart) // 8 bytes

    new ObjectId(bytes.array)
  }

  def getFrom(that: String): Option[ObjectId] =
    if (ObjectId.isValid(that))
      Hex.decode(that).map(new ObjectId(_))
    else
      None

  def getFrom(that: Array[Byte]): Option[ObjectId] =
    if (ObjectId.isValid(that))
      Some(new ObjectId(that))
    else
      None
}
