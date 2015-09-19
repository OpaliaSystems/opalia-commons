package systems.opalia.commons.time

import org.joda.time.{Period => JodaPeriod}
import scala.concurrent.duration._


class Interval(x: Instant, y: Instant) {

  def this(x: Instant, y: FiniteDuration) =
    this(x, x + y)

  val start = Instant.min(x, y)
  val end = Instant.max(x, y)

  private val underlying = new JodaPeriod(start.toMillis, end.toMillis)

  override def equals(that: Any): Boolean =
    that match {

      case (that: Interval) if (this.start == that.start && this.end == that.end) => true
      case _ => false
    }

  override def toString: String =
    start.toDuration.toString + ", " + end.toDuration.toString

  def toDuration: FiniteDuration =
    (end.toMillis - start.toMillis).millis

  def getMillis: Int =
    underlying.getMillis

  def getSeconds: Int =
    underlying.getSeconds

  def getMinutes: Int =
    underlying.getMinutes

  def getHours: Int =
    underlying.getHours

  def getDays: Int =
    underlying.getDays

  def getWeeks: Int =
    underlying.getWeeks

  def getMonths: Int =
    underlying.getMonths

  def getYears: Int =
    underlying.getYears

  def isOverlapped(that: Interval): Boolean =
    this.start < that.end && this.end > that.start

  def isBetween(that: Interval): Boolean =
    this.start >= that.start && this.end <= that.end

  def isBordered(that: Interval): Boolean =
    this.start == that.end || this.end == that.start

  def getOverlappedPart(that: Interval): Option[Interval] =
    if (isOverlapped(that))
      Some(new Interval(Instant.max(this.start, that.start), Instant.min(this.end, that.end)))
    else
      None

  def getGappedPart(that: Interval): Option[Interval] =
    if (!isOverlapped(that))
      Some(new Interval(Instant.max(this.start, that.start), Instant.min(this.end, that.end)))
    else
      None

  def getJoinedPart(that: Interval): Option[Interval] =
    if (isOverlapped(that) || isBordered(that))
      Some(new Interval(Instant.min(this.start, that.start), Instant.max(this.end, that.end)))
    else
      None
}
