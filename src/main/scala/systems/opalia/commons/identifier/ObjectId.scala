package systems.opalia.commons.identifier

import java.net.NetworkInterface
import java.nio.ByteBuffer
import java.util.Objects
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConverters._
import scala.util.Random
import systems.opalia.commons.application.SystemProperty
import systems.opalia.commons.codec.Hex
import systems.opalia.interfaces.identifier._
import systems.opalia.interfaces.rendering._


class ObjectId private(protected val data: Vector[Byte])
  extends Identifier {

  def renderString(renderer: StringRenderer): StringRenderer = {

    renderer ~ Hex.encode(data)
  }
}

object ObjectId
  extends IdentifierCompanion[ObjectId] {

  private object Generator {

    val counter = new AtomicInteger(Random.nextInt())

    def machinePart: Int = {

      val hardwareAddresses =
        NetworkInterface.getNetworkInterfaces.asScala.toSeq
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

  def isValid(that: String): Boolean =
    Hex.isValid(that) && that.length == length * 2

  def isValid(that: Seq[Byte]): Boolean =
    that.length == length

  def length: Int =
    24

  def getNew: ObjectId = {

    val bytes =
      ByteBuffer.allocate(length)
        .order(Renderer.defaultByteOrder)
        .putInt(Generator.machinePart) // 4 bytes
        .putInt(Generator.processPart) // 4 bytes
        .putInt(Generator.counterPart) // 4 bytes
        .putLong(Generator.timestampPart) // 8 bytes
        .putInt(Generator.randomPart) // 4 bytes

    new ObjectId(bytes.array.toVector)
  }

  def getFrom(that: String): ObjectId =
    getFromOpt(that)
      .getOrElse(throw new IllegalArgumentException(s"Cannot generate ObjectId from: $that"))

  def getFrom(that: Seq[Byte]): ObjectId =
    getFromOpt(that)
      .getOrElse(throw new IllegalArgumentException(s"Cannot generate ObjectId from: $that"))

  def getFromOpt(that: String): Option[ObjectId] =
    if (ObjectId.isValid(that))
      Hex.decodeOpt(that).map((x) => new ObjectId(x.toVector))
    else
      None

  def getFromOpt(that: Seq[Byte]): Option[ObjectId] =
    if (ObjectId.isValid(that))
      Some(new ObjectId(that.toVector))
    else
      None
}
