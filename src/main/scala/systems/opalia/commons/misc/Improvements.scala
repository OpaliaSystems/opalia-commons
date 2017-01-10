package systems.opalia.commons.misc

import java.nio.file.{Path, Paths}
import java.time.temporal.{ChronoUnit, TemporalAmount}
import java.time.{Duration => JDuration, _}
import java.util.concurrent.TimeUnit
import scala.collection.JavaConverters._
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.language.implicitConversions


object Improvements {

  implicit class StringImprovements(x: String) {

    import scala.util.control.Exception._

    def toByteOpt: Option[Byte] =
      catching(classOf[NumberFormatException]) opt x.toByte

    def toShortOpt: Option[Short] =
      catching(classOf[NumberFormatException]) opt x.toShort

    def toIntOpt: Option[Int] =
      catching(classOf[NumberFormatException]) opt x.toInt

    def toLongOpt: Option[Long] =
      catching(classOf[NumberFormatException]) opt x.toLong

    def toFloatOpt: Option[Float] =
      catching(classOf[NumberFormatException]) opt x.toFloat

    def toDoubleOpt: Option[Double] =
      catching(classOf[NumberFormatException]) opt x.toDouble

    def toBigInt: BigInt =
      BigInt(x)

    def toBigIntOpt: Option[BigInt] =
      catching(classOf[NumberFormatException]) opt BigInt(x)

    def toBigDecimal: BigDecimal =
      BigDecimal(x)

    def toBigDecimalOpt: Option[BigDecimal] =
      catching(classOf[NumberFormatException]) opt BigDecimal(x)

    def toBooleanOption: Option[Boolean] =
      if (x.toLowerCase == "true" || x.toLowerCase == "on" || x.toLowerCase == "yes" || x == "1")
        Some(true)
      else if (x.toLowerCase == "false" || x.toLowerCase == "off" || x.toLowerCase == "no" || x == "0")
        Some(false)
      else
        None
  }

  implicit class ByteImprovements(x: Byte) {

    def big: BigDecimal =
      BigDecimal.decimal(x)
  }

  implicit class ShortImprovements(x: Short) {

    def big: BigDecimal =
      BigDecimal.decimal(x)
  }

  implicit class IntImprovements(x: Int) {

    def big: BigDecimal =
      BigDecimal.decimal(x)

    def dayAsPeriod: Period =
      daysAsPeriod

    def daysAsPeriod: Period =
      Period.ofDays(x)

    def weekAsPeriod: Period =
      weeksAsPeriod

    def weeksAsPeriod: Period =
      Period.ofWeeks(x)

    def monthAsPeriod: Period =
      monthsAsPeriod

    def monthsAsPeriod: Period =
      Period.ofMonths(x)

    def yearAsPeriod: Period =
      yearsAsPeriod

    def yearsAsPeriod: Period =
      Period.ofYears(x)
  }

  implicit class LongImprovements(x: Long) {

    def big: BigDecimal =
      BigDecimal.decimal(x)
  }

  implicit class FloatImprovements(x: Float) {

    def big: BigDecimal =
      BigDecimal.decimal(x)
  }

  implicit class DoubleImprovements(x: Double) {

    def big: BigDecimal =
      BigDecimal.decimal(x)
  }

  implicit class JavaPathImprovements(x: Path)
    extends Iterable[Path]
      with Ordered[Path] {

    def absolute: Path =
      if (!x.isAbsolute)
        Paths.get("/").resolve(x)
      else
        x

    def relative: Path =
      if (x.isAbsolute)
        Paths.get("/").relativize(x)
      else
        x

    def common(that: Path): Path = {

      def build(a: List[Path], b: List[Path]): Path =
        (a, b) match {
          case (a +: as, b +: bs) if (a == b) => a.resolve(build(as, bs))
          case _ => Paths.get("")
        }

      if (x.isAbsolute == that.isAbsolute) {

        if (x.isAbsolute)
          build(this.toList, that.toList).absolute
        else
          build(this.toList, that.toList).relative

      } else
        Paths.get("")
    }

    def uncommon(that: Path): Path = {

      val commonSize = this.common(that).size

      if (commonSize == 0)
        x
      else if (commonSize == x.size)
        Paths.get("")
      else
        x.subpath(commonSize, x.size)
    }

    def iterator: Iterator[Path] = {

      if (x == Paths.get(""))
        Nil.iterator
      else
        x.iterator().asScala
    }

    def compare(that: Path): Int =
      x.compareTo(that)
  }

  implicit class ScalaDurationImprovements(x: Duration) {

    def chronoUnit: ChronoUnit =
      x.unit match {
        case TimeUnit.DAYS => ChronoUnit.DAYS
        case TimeUnit.HOURS => ChronoUnit.HOURS
        case TimeUnit.MINUTES => ChronoUnit.MINUTES
        case TimeUnit.SECONDS => ChronoUnit.SECONDS
        case TimeUnit.MILLISECONDS => ChronoUnit.MILLIS
        case TimeUnit.MICROSECONDS => ChronoUnit.MICROS
        case TimeUnit.NANOSECONDS => ChronoUnit.NANOS
      }

    def asJava: JDuration =
      JDuration.of(x.length, x.chronoUnit)
  }

  implicit class JavaDurationImprovements(x: JDuration)
    extends Ordered[JDuration] {

    def +(that: JDuration): JDuration =
      x.plus(that)

    def -(that: JDuration): JDuration =
      x.minus(that)

    def +(that: FiniteDuration): JDuration =
      x.plus(that.length, that.chronoUnit)

    def -(that: FiniteDuration): JDuration =
      x.minus(that.length, that.chronoUnit)

    def *(scalar: Long): JDuration =
      x.multipliedBy(scalar)

    def /(divisor: Long): JDuration =
      x.dividedBy(divisor)

    def unary_! : JDuration =
      x.negated

    def compare(that: JDuration): Int =
      x.compareTo(that)
  }

  implicit class JavaPeriodImprovements(x: Period) {

    def +(that: Period): Period =
      x.plus(that)

    def -(that: Period): Period =
      x.minus(that)

    def *(scalar: Int): Period =
      x.multipliedBy(scalar)

    def unary_! : Period =
      x.negated
  }

  implicit class JavaInstantImprovements(x: Instant)
    extends Ordered[Instant] {

    def +(that: TemporalAmount): Instant =
      x.plus(that)

    def -(that: TemporalAmount): Instant =
      x.minus(that)

    def +(that: FiniteDuration): Instant =
      x.plus(that.length, that.chronoUnit)

    def -(that: FiniteDuration): Instant =
      x.minus(that.length, that.chronoUnit)

    def compare(that: Instant): Int =
      x.compareTo(that)
  }

  implicit class JavaOffsetDateTimeImprovements(x: OffsetDateTime)
    extends Ordered[OffsetDateTime] {

    def +(that: TemporalAmount): OffsetDateTime =
      x.plus(that)

    def -(that: TemporalAmount): OffsetDateTime =
      x.minus(that)

    def +(that: FiniteDuration): OffsetDateTime =
      x.plus(that.length, that.chronoUnit)

    def -(that: FiniteDuration): OffsetDateTime =
      x.minus(that.length, that.chronoUnit)

    def compare(that: OffsetDateTime): Int =
      x.compareTo(that)
  }

  implicit class JavaOffsetTimeImprovements(x: OffsetTime)
    extends Ordered[OffsetTime] {

    def +(that: TemporalAmount): OffsetTime =
      x.plus(that)

    def -(that: TemporalAmount): OffsetTime =
      x.minus(that)

    def +(that: FiniteDuration): OffsetTime =
      x.plus(that.length, that.chronoUnit)

    def -(that: FiniteDuration): OffsetTime =
      x.minus(that.length, that.chronoUnit)

    def compare(that: OffsetTime): Int =
      x.compareTo(that)
  }

}
