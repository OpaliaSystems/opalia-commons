package systems.opalia.commons.time

import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.{ChronoField, IsoFields}
import java.time.{OffsetDateTime, ZoneOffset}


object SimpleDateTimeParser {

  /*
    dateTime            = (dateNormal | dateOrdinal | dateWeek) [timeHours] [offset]
    dateNormal          = yyyy '-' MM '-' dd
    dateOrdinal         = yyyy '-' DDD
    dateWeek            = yyyy '-' 'W' ww '-' e
    timeHours           = 'T' HH ([timeMinutes] | [fraction])
    timeMinutes         = ':' mm ([timeSeconds] | [fraction])
    timeSeconds         = ':' ss [fraction]
  */

  val offset: DateTimeFormatter =
    (new DateTimeFormatterBuilder)
      .appendOffsetId()
      .toFormatter()

  val fraction: DateTimeFormatter =
    (new DateTimeFormatterBuilder)
      .appendFraction(ChronoField.NANO_OF_SECOND, 0, 9, true)
      .toFormatter()

  val timeSeconds: DateTimeFormatter =
    (new DateTimeFormatterBuilder)
      .appendLiteral(':')
      .appendValue(ChronoField.SECOND_OF_MINUTE, 2)
      .appendOptional(fraction)
      .toFormatter()

  val timeMinutes: DateTimeFormatter =
    (new DateTimeFormatterBuilder)
      .appendLiteral(':')
      .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
      .optionalStart()
      .appendOptional(timeSeconds)
      .optionalEnd()
      .optionalStart()
      .appendOptional(fraction)
      .optionalEnd()
      .toFormatter()

  val timeHours: DateTimeFormatter =
    (new DateTimeFormatterBuilder)
      .appendLiteral('T')
      .appendValue(ChronoField.HOUR_OF_DAY, 2)
      .optionalStart()
      .appendOptional(timeMinutes)
      .optionalEnd()
      .optionalStart()
      .appendOptional(fraction)
      .optionalEnd()
      .toFormatter()

  val dateWeek: DateTimeFormatter =
    (new DateTimeFormatterBuilder)
      .parseCaseInsensitive
      .appendValue(IsoFields.WEEK_BASED_YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .appendLiteral("-W")
      .appendValue(IsoFields.WEEK_OF_WEEK_BASED_YEAR, 2)
      .appendLiteral('-')
      .appendValue(ChronoField.DAY_OF_WEEK, 1)
      .toFormatter()

  val dateOrdinal: DateTimeFormatter =
    (new DateTimeFormatterBuilder)
      .appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .appendLiteral('-')
      .appendValue(ChronoField.DAY_OF_YEAR, 3)
      .toFormatter()

  val dateNormal: DateTimeFormatter =
    (new DateTimeFormatterBuilder)
      .appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .appendLiteral('-')
      .appendValue(ChronoField.MONTH_OF_YEAR, 2)
      .appendLiteral('-')
      .appendValue(ChronoField.DAY_OF_MONTH, 2)
      .toFormatter()

  val dateTime: DateTimeFormatter =
    (new DateTimeFormatterBuilder)
      .optionalStart()
      .append(dateNormal)
      .optionalEnd()
      .optionalStart()
      .append(dateOrdinal)
      .optionalEnd()
      .optionalStart()
      .append(dateWeek)
      .optionalEnd()
      .appendOptional(timeHours)
      .appendOptional(offset)
      .toFormatter()

  def parse(string: String): OffsetDateTime = {

    val result =
      dateTime.parse(string)

    val year =
      result.get(ChronoField.YEAR)

    val month =
      result.get(ChronoField.MONTH_OF_YEAR)

    val day =
      result.get(ChronoField.DAY_OF_MONTH)

    val hour =
      if (result.isSupported(ChronoField.HOUR_OF_DAY)) result.get(ChronoField.HOUR_OF_DAY) else 0

    val minute =
      if (result.isSupported(ChronoField.MINUTE_OF_HOUR)) result.get(ChronoField.MINUTE_OF_HOUR) else 0

    val second =
      if (result.isSupported(ChronoField.SECOND_OF_MINUTE)) result.get(ChronoField.SECOND_OF_MINUTE) else 0

    val nano =
      if (result.isSupported(ChronoField.NANO_OF_SECOND)) result.get(ChronoField.NANO_OF_SECOND) else 0

    val offset =
      if (result.isSupported(ChronoField.OFFSET_SECONDS)) result.get(ChronoField.OFFSET_SECONDS) else 0

    OffsetDateTime.of(year, month, day, hour, minute, second, nano, ZoneOffset.ofTotalSeconds(offset))
  }
}
