package systems.opalia.commons.io

import java.nio.file.Paths
import org.scalatest._
import systems.opalia.commons.io.Imports._


class InOutImprovementsTest
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
}
