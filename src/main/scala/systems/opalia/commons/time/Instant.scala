package systems.opalia.commons.time

import scala.concurrent.duration._


class Instant(millis: Long)
  extends Ordered[Instant] {

  def this(that: FiniteDuration) =
    this(that.toMillis)

  override def equals(that: Any): Boolean =
    that match {

      case (that: Instant) if (this.toMillis == that.toMillis) => true
      case _ => false
    }

  override def toString: String =
    this.toDuration.toString

  override def compare(that: Instant): Int =
    (this.toMillis - that.toMillis).toInt

  def +(that: Long): Instant =
    new Instant(this.toMillis + that)

  def -(that: Long): Instant =
    new Instant(this.toMillis - that)

  def +(that: Instant): Instant =
    this + that.toMillis

  def -(that: Instant): Instant =
    this - that.toMillis

  def +(that: FiniteDuration): Instant =
    this + that.toMillis

  def -(that: FiniteDuration): Instant =
    this - that.toMillis

  def toDuration: FiniteDuration =
    this.toMillis.millis

  def toMillis: scala.Long =
    millis

  def toSeconds: Long =
    this.toDuration.toSeconds

  def toMinutes: Long =
    this.toDuration.toMinutes

  def toHours: Long =
    this.toDuration.toHours

  def toDays: Long =
    this.toDuration.toDays
}

object Instant
  extends Ordering[Instant] {

  override def compare(x: Instant, y: Instant): Int =
    x.compare(y)
}
