package systems.opalia.commons.time

import java.time.{OffsetTime, ZoneOffset, Duration => JDuration}
import org.scalatest._
import scala.concurrent.duration._
import scala.language.postfixOps


class SimpleTimeParserTest
  extends FlatSpec
    with Matchers {

  it should "parse different formats" in {

    val timeParts =
      List(
        ("04:15:30", ((4 hours) + (15 minutes) + (30 seconds)).toMillis),
        ("04:15:30.432", ((4 hours) + (15 minutes) + (30 seconds) + (432 milliseconds)).toMillis))

    val offsetParts =
      List(
        ("", 0l),
        ("+01:00", -(1 hour).toMillis),
        ("+01:30", -((1 hour) + (30 minutes)).toMillis),
        ("+01:30:20", -((1 hour) + (30 minutes) + (20 seconds)).toMillis),
        ("-08:00", (8 hours).toMillis),
        ("Z", 0l))

    for (a <- timeParts; b <- offsetParts) {

      JDuration.between(OffsetTime.of(0, 0, 0, 0, ZoneOffset.UTC), SimpleTimeParser.parse(a._1 + b._1)) should
        be(JDuration.ofMillis(a._2 + b._2))
    }
  }
}
