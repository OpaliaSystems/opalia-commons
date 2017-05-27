package systems.opalia.commons.time

import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField
import java.time.{OffsetTime, ZoneOffset}


object SimpleTimeParser {

  /*
    time = HH ':' mm ':' ss [fraction] [offset]
  */

  val offset: DateTimeFormatter =
    (new DateTimeFormatterBuilder)
      .appendOffsetId()
      .toFormatter()

  val fraction: DateTimeFormatter =
    (new DateTimeFormatterBuilder)
      .appendFraction(ChronoField.NANO_OF_SECOND, 0, 9, true)
      .toFormatter()

  val time: DateTimeFormatter =
    (new DateTimeFormatterBuilder)
      .appendValue(ChronoField.HOUR_OF_DAY, 2)
      .appendLiteral(':')
      .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
      .appendLiteral(':')
      .appendValue(ChronoField.SECOND_OF_MINUTE, 2)
      .appendOptional(fraction)
      .appendOptional(offset)
      .toFormatter()

  def parse(string: String): OffsetTime = {

    val result =
      time.parse(string)

    val hour =
      result.get(ChronoField.HOUR_OF_DAY)

    val minute =
      result.get(ChronoField.MINUTE_OF_HOUR)

    val second =
      result.get(ChronoField.SECOND_OF_MINUTE)

    val nano =
      if (result.isSupported(ChronoField.NANO_OF_SECOND)) result.get(ChronoField.NANO_OF_SECOND) else 0

    val offset =
      if (result.isSupported(ChronoField.OFFSET_SECONDS)) result.get(ChronoField.OFFSET_SECONDS) else 0

    OffsetTime.of(hour, minute, second, nano, ZoneOffset.ofTotalSeconds(offset))
  }
}
