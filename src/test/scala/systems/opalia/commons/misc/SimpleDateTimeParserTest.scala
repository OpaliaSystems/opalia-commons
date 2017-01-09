package systems.opalia.commons.misc

import java.time.{Instant, OffsetDateTime, ZoneOffset}
import org.scalatest._
import scala.concurrent.duration._
import scala.language.postfixOps


class SimpleDateTimeParserTest
  extends FlatSpec
    with Matchers {

  it should "parse different types of date" in {

    val timestamp =
      OffsetDateTime.of(2011, 2, 3, 0, 0, 0, 0, ZoneOffset.UTC).toInstant.toEpochMilli

    val dateNormal =
      SimpleDateTimeParser.parse("2011-02-03").toInstant.toEpochMilli

    val dateOrdinal =
      SimpleDateTimeParser.parse("2011-034").toInstant.toEpochMilli

    val dateWeek =
      SimpleDateTimeParser.parse("2011-W05-4").toInstant.toEpochMilli

    dateNormal should be(timestamp)
    dateOrdinal should be(timestamp)
    dateWeek should be(timestamp)
  }

  it should "parse different formats" in {

    val timestamp =
      OffsetDateTime.of(2011, 2, 3, 0, 0, 0, 0, ZoneOffset.UTC).toInstant.toEpochMilli

    val dateParts =
      List(
        ("2011-02-03", timestamp),
        ("2011-034", timestamp),
        ("2011-W05-4", timestamp))

    val timeParts =
      List(
        ("", 0l),
        ("T04", (4 hours).toMillis),
        ("T04:15", ((4 hours) + (15 minutes)).toMillis),
        ("T04:15:30", ((4 hours) + (15 minutes) + (30 seconds)).toMillis),
        ("T04:15:30.432", ((4 hours) + (15 minutes) + (30 seconds) + (432 milliseconds)).toMillis),
        ("T04:15.432", ((4 hours) + (15 minutes) + (432 milliseconds)).toMillis),
        ("T04.432", ((4 hours) + (432 milliseconds)).toMillis))

    val offsetParts =
      List(
        ("", 0l),
        ("+01:00", -(1 hour).toMillis),
        ("+01:30", -((1 hour) + (30 minutes)).toMillis),
        ("+01:30:20", -((1 hour) + (30 minutes) + (20 seconds)).toMillis),
        ("-08:00", (8 hours).toMillis),
        ("Z", 0l))

    for (a <- dateParts; b <- timeParts; c <- offsetParts) {

      SimpleDateTimeParser.parse(a._1 + b._1 + c._1).toInstant should
        be(Instant.ofEpochMilli(a._2 + b._2 + c._2))
    }
  }
}
