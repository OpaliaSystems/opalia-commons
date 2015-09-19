package systems.opalia.commons.time

import org.joda.time.format._
import org.joda.time.{DateTime => JodaDateTime}


class DateTime(millis: Long)
  extends Instant(millis) {

  def this(that: Instant) =
    this(that.toMillis)

  private val underlying = new JodaDateTime(millis)

  def getMillisOfSecond: Int =
    underlying.millisOfSecond.get

  def getMillisOfDay: Int =
    underlying.millisOfDay.get

  def getSecondOfMinute: Int =
    underlying.secondOfMinute.get

  def getSecondOfDay: Int =
    underlying.secondOfDay().get

  def getMinuteOfHour: Int =
    underlying.minuteOfHour.get

  def getMinuteOfDay: Int =
    underlying.minuteOfDay.get

  def getHourOfDay: Int =
    underlying.hourOfDay.get

  def getDayOfWeek: Int =
    underlying.dayOfWeek().get

  def getDayOfMonth: Int =
    underlying.dayOfMonth.get

  def getDayOfYear: Int =
    underlying.dayOfYear.get

  def getWeekOfYear: Int =
    underlying.weekOfWeekyear().get

  def getMonthOfYear: Int =
    underlying.monthOfYear.get

  def getYear: Int =
    underlying.year.get

  def changeMillis(x: Int): DateTime =
    if (x < 0)
      new DateTime(underlying.minusMillis(x).getMillis)
    else
      new DateTime(underlying.plusMillis(x).getMillis)

  def changeSecond(x: Int): DateTime =
    if (x < 0)
      new DateTime(underlying.minusSeconds(x).getMillis)
    else
      new DateTime(underlying.plusSeconds(x).getMillis)

  def changeMinute(x: Int): DateTime =
    if (x < 0)
      new DateTime(underlying.minusMinutes(x).getMillis)
    else
      new DateTime(underlying.plusMinutes(x).getMillis)

  def changeHour(x: Int): DateTime =
    if (x < 0)
      new DateTime(underlying.minusHours(x).getMillis)
    else
      new DateTime(underlying.plusHours(x).getMillis)

  def changeDay(x: Int): DateTime =
    if (x < 0)
      new DateTime(underlying.minusDays(x).getMillis)
    else
      new DateTime(underlying.plusDays(x).getMillis)

  def changeWeeks(x: Int): DateTime =
    if (x < 0)
      new DateTime(underlying.minusWeeks(x).getMillis)
    else
      new DateTime(underlying.plusWeeks(x).getMillis)

  def changeMonth(x: Int): DateTime =
    if (x < 0)
      new DateTime(underlying.minusMonths(x).getMillis)
    else
      new DateTime(underlying.plusMonths(x).getMillis)

  def changeYear(x: Int): DateTime =
    if (x < 0)
      new DateTime(underlying.minusYears(x).getMillis)
    else
      new DateTime(underlying.plusYears(x).getMillis)

  def withMillisOfSecond(x: Int): DateTime =
    new DateTime(underlying.withMillisOfSecond(x).getMillis)

  def withMillisOfDay(x: Int): DateTime =
    new DateTime(underlying.withMillisOfDay(x).getMillis)

  def withSecondOfMinute(x: Int): DateTime =
    new DateTime(underlying.withSecondOfMinute(x).getMillis)

  def withMinuteOfHour(x: Int): DateTime =
    new DateTime(underlying.withMinuteOfHour(x).getMillis)

  def withHourOfDay(x: Int): DateTime =
    new DateTime(underlying.withHourOfDay(x).getMillis)

  def withDayOfWeek(x: Int): DateTime =
    new DateTime(underlying.withDayOfWeek(x).getMillis)

  def withDayOfMonth(x: Int): DateTime =
    new DateTime(underlying.withDayOfMonth(x).getMillis)

  def withDayOfYear(x: Int): DateTime =
    new DateTime(underlying.withDayOfYear(x).getMillis)

  def withMonthOfYear(x: Int): DateTime =
    new DateTime(underlying.withMonthOfYear(x).getMillis)

  def withYear(year: Int): DateTime =
    new DateTime(underlying.withYear(year).getMillis)
}

object DateTime {

  def now: DateTime =
    new DateTime(JodaDateTime.now().getMillis)

  def zero: DateTime =
    new DateTime(0)

  def format(data: DateTime, format: String): String = {

    val formatter = DateTimeFormat.forPattern(format)
    val jodaDateTime = new JodaDateTime(data.toMillis)

    formatter.print(jodaDateTime)
  }

  def parse(data: String, format: String*): DateTime = {

    val builder = new DateTimeFormatterBuilder()

    builder.append(null, format.map((x) => DateTimeFormat.forPattern(x).getParser).toArray)

    val formatter = builder.toFormatter
    val jodaDateTime = formatter.parseDateTime(data)

    new DateTime(jodaDateTime.getMillis)
  }

  def parseIso(data: String): DateTime = {

    val jodaDateTime = ISODateTimeFormat.dateTimeParser().parseDateTime(data)

    new DateTime(jodaDateTime.getMillis)
  }
}
