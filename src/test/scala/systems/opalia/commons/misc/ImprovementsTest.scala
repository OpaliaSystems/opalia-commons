package systems.opalia.commons.misc

import java.nio.file.Paths
import java.time.temporal.ChronoUnit
import java.time.{Duration => JDuration, _}
import org.scalatest._
import scala.concurrent.duration._
import scala.language.postfixOps
import systems.opalia.commons.misc.Improvements._


class ImprovementsTest
  extends FlatSpec
    with Matchers {

  it should "be able to find common path (java.nio.file.Paths)" in {

    val p1 = Paths.get("/home/joe")
    val p2 = Paths.get("/home/joe/foo")
    val p3 = Paths.get("/home/joe/bar")

    val p4 = Paths.get("")

    p1.common(p1) should be(p1)

    p1.common(p3) should be(p1)

    p2.common(p3) should be(p1)

    p1.relative.common(p3.relative) should be(p1.relative)

    p2.relative.common(p3.relative) should be(p1.relative)

    p2.common(p3.relative) should be(p4)
  }

  it should "be able to find uncommon path (java.nio.file.Paths)" in {

    val p1 = Paths.get("/home/joe/foo/bar")
    val p2 = Paths.get("/home/joe")
    val p3 = Paths.get("foo/bar")

    val p4 = Paths.get("/home/joe/foo")
    val p5 = Paths.get("/home/joe/bar")
    val p6 = Paths.get("foo")
    val p7 = Paths.get("bar")

    val p8 = Paths.get("")

    p1.uncommon(p1) should be(p8)

    p1.uncommon(p1.relative) should be(p1)

    p1.uncommon(p2) should be(p3)

    p4.uncommon(p5) should be(p6)

    p5.uncommon(p4) should be(p7)
  }

  it should "be easier to use (java.time.Duration)" in {

    val duration1 = JDuration.ofNanos(999)
    val duration2 = duration1.plus(3, ChronoUnit.DAYS)

    val scalaDuration = 3 days
    val javaDuration = (3 days).asJava

    (duration1 + scalaDuration) should be(duration2)
    (duration1 + javaDuration) should be(duration2)

    (duration1 * 2) should be((1998 nanoseconds).asJava)
    (duration1 / 2) should be((499 nanoseconds).asJava)
    (!duration1) should be((-999 nanoseconds).asJava)

    duration1 == duration2 should be(false)
    duration1 != duration2 should be(true)
    duration1 < duration2 should be(true)
    duration1 <= duration2 should be(true)
    duration1 > duration2 should be(false)
    duration1 >= duration2 should be(false)
  }

  it should "be easier to use (java.time.Period)" in {

    val period = 28 weeksAsPeriod

    (period + period) should be(56 weeksAsPeriod)

    (period * 2) should be(56 weeksAsPeriod)
    (!period) should be(-28 weeksAsPeriod)
  }

  it should "be easier to use (java.time.Instant)" in {

    val instant1 = Instant.EPOCH
    val instant2 = instant1.plus(73, ChronoUnit.DAYS)

    val scalaDuration = 73 days
    val javaDuration = (73 days).asJava
    val javaPeriod = 73 daysAsPeriod

    (instant1 + scalaDuration) should be(instant2)
    (instant1 + javaDuration) should be(instant2)
    (instant1 + javaPeriod) should be(instant2)

    instant1 == instant2 should be(false)
    instant1 != instant2 should be(true)
    instant1 < instant2 should be(true)
    instant1 <= instant2 should be(true)
    instant1 > instant2 should be(false)
    instant1 >= instant2 should be(false)
  }

  it should "be easier to use (java.time.OffsetDateTime)" in {

    val dateTime1 = Instant.EPOCH.atOffset(ZoneOffset.UTC)
    val dateTime2 = dateTime1.plus(42, ChronoUnit.DAYS)

    val scalaDuration = 42 days
    val javaDuration = (42 days).asJava
    val javaPeriod = 42 daysAsPeriod

    (dateTime1 + scalaDuration) should be(dateTime2)
    (dateTime1 + javaDuration) should be(dateTime2)
    (dateTime1 + javaPeriod) should be(dateTime2)

    dateTime1 == dateTime2 should be(false)
    dateTime1 != dateTime2 should be(true)
    dateTime1 < dateTime2 should be(true)
    dateTime1 <= dateTime2 should be(true)
    dateTime1 > dateTime2 should be(false)
    dateTime1 >= dateTime2 should be(false)
  }

  it should "be easier to use (java.time.OffsetTime)" in {

    val time1 = OffsetTime.of(0, 0, 0, 0, ZoneOffset.UTC)
    val time2 = time1.plus(42, ChronoUnit.HOURS)

    val scalaDuration = 42 hours
    val javaDuration = (42 hours).asJava

    (time1 + scalaDuration) should be(time2)
    (time1 + javaDuration) should be(time2)

    time1 == time2 should be(false)
    time1 != time2 should be(true)
    time1 < time2 should be(true)
    time1 <= time2 should be(true)
    time1 > time2 should be(false)
    time1 >= time2 should be(false)
  }
}
