package systems.opalia.commons.net

import org.scalatest.flatspec._
import org.scalatest.matchers.should._


class PathMatchingTest
  extends AnyFlatSpec
    with Matchers {

  it should "be able to check for collision of parameter paths" in {

    val p0 = PathMatching.ParameterPath("/a/?x")

    p0.collides(PathMatching.ParameterPath("/a/?x")) shouldBe true
    p0.collides(PathMatching.ParameterPath("/a/?y")) shouldBe true
    p0.collides(PathMatching.ParameterPath("/a/z")) shouldBe true
    p0.collides(PathMatching.ParameterPath("/?a/z")) shouldBe true
    p0.collides(PathMatching.ParameterPath("/?b/z")) shouldBe true
    p0.collides(PathMatching.ParameterPath("/?b/?z")) shouldBe true

    p0.collides(PathMatching.ParameterPath("/a/?x/")) shouldBe false
    p0.collides(PathMatching.ParameterPath("/b/z")) shouldBe false
    p0.collides(PathMatching.ParameterPath("/b/?z")) shouldBe false
    p0.collides(PathMatching.ParameterPath("/a")) shouldBe false
    p0.collides(PathMatching.ParameterPath("/?a")) shouldBe false
    p0.collides(PathMatching.ParameterPath("/a/z/a")) shouldBe false
    p0.collides(PathMatching.ParameterPath("/a/?z/a")) shouldBe false
    p0.collides(PathMatching.ParameterPath("/?a/z/a")) shouldBe false
    p0.collides(PathMatching.ParameterPath("/?a/?z/a")) shouldBe false
  }

  it should "be able to match a parameter path with an argument path" in {

    val p0 = PathMatching.ParameterPath("/a/b/c/d")
    val p1 = PathMatching.ParameterPath("/a/b//c/d/")
    val p2 = PathMatching.ParameterPath("/a/?b/c/?d<(d|x)+>/?e")

    p0.matches(PathMatching.ArgumentPath("/a/b/c/d")) shouldBe true
    p0.matches(PathMatching.ArgumentPath("/a/b/c/d/")) shouldBe false
    p0.matches(PathMatching.ArgumentPath("/a/b/c/f")) shouldBe false

    p1.matches(PathMatching.ArgumentPath("/a/b//c/d/")) shouldBe true
    p1.matches(PathMatching.ArgumentPath("/a/b/c/d/")) shouldBe false
    p1.matches(PathMatching.ArgumentPath("/a/b//c/d")) shouldBe false

    p2.matches(PathMatching.ArgumentPath("/a/b/c/d/e")) shouldBe true
    p2.matches(PathMatching.ArgumentPath("/a/bx/c/dx/ex")) shouldBe true
    p2.matches(PathMatching.ArgumentPath("/a/bx/c/dy/ex")) shouldBe false
    p2.matches(PathMatching.ArgumentPath("/a/bx/cx/dx/ex")) shouldBe false
  }

  it should "be able to arguments from argument path" in {

    val p0 = PathMatching.ParameterPath("/a/?b/c/?d<(a|b)*>/?e")

    p0.arguments(PathMatching.ArgumentPath("/1/42/3/4/5")) shouldBe Map("b" -> "42", "d" -> "4", "e" -> "5")
    p0.arguments(PathMatching.ArgumentPath("/1/2/3/4/")) shouldBe Map("b" -> "2", "d" -> "4", "e" -> "")
  }

  it should "serialize the path objects back to strings" in {

    PathMatching.ParameterPath("/1/2//?4/5/").toString shouldBe "/1/2//?4/5/"
    PathMatching.ParameterPath("/1/2//?4<(a|b){4,8}>/5").toString shouldBe "/1/2//?4<(a|b){4,8}>/5"
    PathMatching.ArgumentPath("/1%C3%A4/2//4/5/").toString shouldBe "/1%C3%A4/2//4/5/"
    PathMatching.ArgumentPath("/1/2/3").toString shouldBe "/1/2/3"
  }

  it should "throw an exception while parsing invalid parameter paths" in {

    an[IllegalArgumentException] should be thrownBy PathMatching.ParameterPath("a/b")
    an[IllegalArgumentException] should be thrownBy PathMatching.ParameterPath("/a/?/c")
    an[IllegalArgumentException] should be thrownBy PathMatching.ParameterPath("/a/?b<>")
    an[IllegalArgumentException] should be thrownBy PathMatching.ParameterPath("/a/?b<\\s{1, 2, 3}>/")
    an[IllegalArgumentException] should be thrownBy PathMatching.ParameterPath("/a/b?/c")
  }
}
