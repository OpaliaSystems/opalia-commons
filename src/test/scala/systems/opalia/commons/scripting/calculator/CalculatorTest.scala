package systems.opalia.commons.scripting.calculator

import org.scalatest._
import systems.opalia.commons.scripting.JavaScript


class CalculatorTest
  extends FlatSpec
    with Matchers {

  it should "deal with simple declarations" in {

    val js = JavaScript()
    val calc = new Calculator(js)

    calc.bindDefaultFunctions()

    calc.bindFunctions(
      """
        |const1 : 42
        |const2 : -const1
        |
      """.stripMargin)

    calc.eval("const1 = 42").value() shouldBe 1d
    calc.eval("const2 = -42").value() shouldBe 1d
    calc.eval("-const2 = 42").value() shouldBe 1d
    calc.eval("-(const2) = 42").value() shouldBe 1d
    calc.eval("(42) = 42").value() shouldBe 1d
    calc.eval("+(42) = +42").value() shouldBe 1d
    calc.eval("-(42) = -42").value() shouldBe 1d
  }

  it should "handle operator priority correctly" in {

    val js = JavaScript()
    val calc = new Calculator(js)

    calc.bindDefaultFunctions()

    calc.bindFunctions(
      """
        | foo : 5 + -2^3
        | bar : 5 + -2*3
        | baz : 5 * -2+3
        |
      """.stripMargin)

    calc.eval("foo").value() shouldBe -3d
    calc.eval("bar").value() shouldBe -1d
    calc.eval("baz").value() shouldBe -7d
  }

  it should "support single line comments" in {

    val js = JavaScript()
    val calc = new Calculator(js)

    calc.bindDefaultFunctions()

    calc.eval(
      """
        |1 / (   # comment here
        |  1 + 9
        |) * 10  # * 100
        |
      """.stripMargin).value() shouldBe 1d
  }

  it should "support multiple declarations per line" in {

    val js = JavaScript()
    val calc = new Calculator(js)

    calc.bindDefaultFunctions()

    calc.bindFunctions(
      """
        |foo : 42; bar : 73
        |baz : 0;
        |
      """.stripMargin)

    calc.eval("foo").value() shouldBe 42d
    calc.eval("bar").value() shouldBe 73d
    calc.eval("baz").value() shouldBe 0d
  }

  it should "support comparison operators" in {

    val js = JavaScript()
    val calc = new Calculator(js)

    calc.bindDefaultFunctions()

    calc.bindFunctions(
      """
        |a0 : (1 < 2) - (2 < 1)
        |b0 : (2 > 1) - (1 > 2)
        |c0 : (1 = 1) - (1 = 2)
        |d0 : (1 <= 1) + (1 <= 2) - (2 <= 1)
        |d1 : (1 ≤ 1) + (1 ≤ 2) - (2 ≤ 1)
        |e0 : (1 >= 1) + (2 >= 1) - (1 >= 2)
        |e1 : (1 ≥ 1) + (2 ≥ 1) - (1 ≥ 2)
        |f0 : (1 <> 2) - (1 <> 1)
        |f1 : (1 ≠ 2) - (1 ≠ 1)
        |
      """.stripMargin)

    calc.eval("a0 + b0 + c0 + d0 + e0 + f0").value() shouldBe 8d
    calc.eval("d1 + e1 + f1").value() shouldBe 5d
  }

  it should "be free from name collisions" in {

    val js = JavaScript()
    val calc = new Calculator(js)

    calc.bindDefaultFunctions()

    calc.bindFunctions(
      """
        |foo a b : bar (a + b) 2
        |
        |bar a b : baz (a + b) 3
        |
        |baz a b : a + b
        |
      """.stripMargin
    )

    calc.getFunction("foo").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(5)),
      FunctionApp.wrap(FunctionApp.fromDouble(9))
    )).value() shouldBe 19d
  }

  it should "support higher order functions" in {

    val js = JavaScript()
    val calc = new Calculator(js)

    calc.bindDefaultFunctions()

    calc.bindFunctions(
      """
        |foo a b : a * b
        |
        |bar (x a b) : x 2 3
        |
        |baz y z : y + z
        |
      """.stripMargin)

    calc.eval("foo 2 3").value() shouldBe 6d
    calc.eval("bar foo").value() shouldBe 6d
    calc.eval("baz (bar foo) 4").value() shouldBe 10d
  }

  it should "handle signatures correctly" in {

    val js = JavaScript()
    val calc = new Calculator(js)

    calc.bindDefaultFunctions()

    calc.bindFunctions(
      """
        |f01 : 5
        |f02 a : a
        |f03 a b : a + b
        |f04 a (b x y) : b a 1
        |f05 \ (n x) : (x . x + 1)
        |f06 \ (n x y) : (x y . x + y)
        |f07 a \ (n x y) : (x y . a + x + y)
        |f08 a b \ (n x y) : (x y . a + b + x + y)
        |f09 (a x y) \ (n x y) : a
        |f10 (a \ (z x)) (b x y) : b ((a) 1) 3
        |f11 (a \ (z x)) \ (n x y) : (x y . ((a) x) + y)
        |f12 (a \ (z x)) \ (n \ (z x)) : a
        |f13 (a \ (z x)) : ((a) 1) + 3
        |f14 (a x y \ (z x y)) : (a 1 3) 5 10
        |f15 (a x y \ (z x y)) \ (n x y) : a 1 3
        |f16 (a x (y x \ (z p q))) \ (n x y) : (n m . a 1 (x \ (z p q) . (p q . x + p + q + n + m)))
        |
      """.stripMargin)

    calc.eval("f01").value() shouldBe 5d
    calc.eval("f02 3").value() shouldBe 3d
    calc.eval("f03 3 5").value() shouldBe 8d
    calc.eval("f04 3 f03").value() shouldBe 4d
    calc.eval("(f05) 3").value() shouldBe 4d
    calc.eval("(f06) 3 5").value() shouldBe 8d
    calc.eval("(f07 1) 3 5").value() shouldBe 9d
    calc.eval("(f08 1 10) 3 5").value() shouldBe 19d
    calc.eval("(f09 f03) 3 5").value() shouldBe 8d
    calc.eval("f10 f05 f03").value() shouldBe 5d
    calc.eval("(f11 f05) 3 5").value() shouldBe 9d
    calc.eval("((f12 f05)) 1").value() shouldBe 2d
    calc.eval("f13 f05").value() shouldBe 5d
    calc.eval("f14 f08").value() shouldBe 19d
    calc.eval("(f15 f08) 5 10").value() shouldBe 19d
    calc.eval("(f16 (a (b x \\ (z p q)) . ((b 3) a 10))) 7 4").value() shouldBe 25d
    calc.eval("f04 1 (f06)").value() shouldBe 2d
  }

  it should "support recursion" in {

    val js = JavaScript()
    val calc = new Calculator(js)

    calc.bindDefaultFunctions()

    calc.bindFunctions(
      """
        |foo a b :
        |  if (b = 0)
        |    a
        |    (foo (a + b) (b - 1))
        |
      """.stripMargin
    )

    calc.getFunction("foo").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(3)),
      FunctionApp.wrap(FunctionApp.fromDouble(3))
    )).value() shouldBe 9d
  }

  it should "support lambda expressions" in {

    val js = JavaScript()
    val calc = new Calculator(js)

    calc.bindDefaultFunctions()

    calc.bindFunctions(
      """
        |foo a b : a * b
        |
        |bar (f a b) c : f c 2
        |
        |baz c (g (f a b) c) (f a b) : g f c
        |
        |bla c (g (f a b) c) : g foo c
        |
      """.stripMargin)

    calc.eval("foo 2 3").value() shouldBe 6d
    calc.eval("(a b . a * b) 2 3").value() shouldBe 6d

    calc.eval("bar foo 3").value() shouldBe 6d
    calc.eval("bar (a b . a * b) 3").value() shouldBe 6d
    calc.eval("((f a b) c . f c 2) (a b . a * b) 3").value() shouldBe 6d

    calc.eval("baz 3 bar foo").value() shouldBe 6d
    calc.eval("baz 3 bar (a b . a * b)").value() shouldBe 6d
    calc.eval("baz 3 ((f a b) c . f c 2) (a b . a * b)").value() shouldBe 6d
    calc.eval("(c (g (f a b) c) (f a b) . g f c) 3 ((f a b) c . f c 2) (a b . a * b)").value() shouldBe 6d

    calc.eval("bla 3 bar").value() shouldBe 6d
    calc.eval("bla 3 ((f a b) c . f c 2)").value() shouldBe 6d
    calc.eval("(c (g (f a b) c) . g foo c) 3 ((f a b) c . f c 2)").value() shouldBe 6d
    calc.eval("(c (g (f a b) c) . g (a b . a * b) c) 3 ((f a b) c . f c 2)").value() shouldBe 6d
  }

  it should "be possible to handle interim results with lambda expressions" in {

    val js = JavaScript()
    val calc = new Calculator(js)

    calc.bindDefaultFunctions()

    calc.bindFunctions(
      """
        |foo x : (a b c . a * a * a + b * b * b + c * c * c) (sin x) (cos x) (tan x)
        |
      """.stripMargin)

    calc.getFunction("foo").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(42))
    )).value() should be(11.1969567325 +- 0.000000000003)
  }

  it should "be able to deal with real world problems" in {

    val js = JavaScript()
    val calc = new Calculator(js)

    calc.bindDefaultFunctions()

    calc.bindFunctions(
      """
        |gcd a b :
        |  if ((isWhole a = 0) + (isWhole b = 0))
        |    NaN
        |    (if (b = 0)
        |      a
        |      (gcd b (a % b))
        |    )
        |
        |lcm a b :
        |  if ((isWhole a = 0) + (isWhole b = 0))
        |    NaN
        |    (a * b / gcd a b)
        |
        |fac n :
        |  if ((isWhole n = 0) + (n < 0))
        |    NaN
        |    (if (n = 0)
        |      1
        |      (n * fac (n - 1))
        |    )
        |
        |fac_tail_rec n :
        |  if ((isWhole n = 0) + (n < 0))
        |    NaN
        |    (fac_tail_rec_2 1 n)
        |
        |fac_tail_rec_2 s n :
        |  if (n = 0)
        |    s
        |    (fac_tail_rec_2 (s * n) (n - 1))
        |
        |fib n :
        |  if ((isWhole n = 0) + (n < 0))
        |    NaN
        |    (if (n < 2)
        |      n
        |      (fib (n - 1) + fib (n - 2))
        |    )
        |
        |fib_tail_rec n :
        |  if ((isWhole n = 0) + (n < 0))
        |    NaN
        |    (fib_tail_rec_3 0 1 n)
        |
        |fib_tail_rec_3 a b n :
        |  if (n = 0)
        |    a
        |    (fib_tail_rec_3 b (a + b) (n - 1))
        |
        |stirling1 n r :
        |  if ((r = 0) * (0 < n) + (n < r))
        |    0
        |    (if ((n = 1) + (n = r))
        |      1
        |      ((n - 1) * stirling1 (n - 1) r + stirling1 (n - 1) (r - 1))
        |    )
        |
        |stirling2 n r :
        |  if ((r = 0) * (0 < n) + (n < r))
        |    0
        |    (if ((n = 1) + (n = r))
        |      1
        |      (r * stirling2 (n - 1) r + stirling2 (n - 1) (r - 1))
        |    )
        |
        |digitsum n :
        |  if (isWhole n = 0)
        |    NaN
        |    (if (n < 0)
        |      -(digitsum_2 -n 0)
        |      (digitsum_2 n 0)
        |    )
        |
        |digitsum_2 n m :
        |    (if (n = 0)
        |      m
        |      (digitsum_2 (floor (n / 10)) (n % 10 + m))
        |    )
        |
        |is_prime n :
        |  if (n <= 1)
        |    0
        |    (if ((n = 2) + (n = 3))
        |      1
        |      (if (n % 2 = 0)
        |        0
        |        (div_odd n 3)
        |      )
        |    )
        |
        |div_odd n i :
        |  if (n % i = 0)
        |    0
        |    (if (i * i <= n)
        |      (div_odd n (i + 2))
        |      1
        |    )
        |
      """.stripMargin)

    calc.getFunction("gcd").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(24)),
      FunctionApp.wrap(FunctionApp.fromDouble(32))
    )).value() shouldBe 8d

    calc.getFunction("lcm").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(3)),
      FunctionApp.wrap(FunctionApp.fromDouble(8))
    )).value() shouldBe 24d

    calc.getFunction("fac").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(7))
    )).value() shouldBe 5040d

    calc.getFunction("fac").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(7.3))
    )).value().isNaN shouldBe true

    calc.getFunction("fac_tail_rec").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(7))
    )).value() shouldBe 5040d

    calc.getFunction("fib").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(20))
    )).value() shouldBe 6765d

    calc.getFunction("fib_tail_rec").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(20))
    )).value() shouldBe 6765d

    calc.getFunction("stirling1").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(8)),
      FunctionApp.wrap(FunctionApp.fromDouble(4))
    )).value() shouldBe 6769d

    calc.getFunction("stirling2").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(9)),
      FunctionApp.wrap(FunctionApp.fromDouble(5))
    )).value() shouldBe 6951d

    calc.getFunction("digitsum").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(42))
    )).value() shouldBe 6d

    calc.getFunction("digitsum").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(-73))
    )).value() shouldBe -10d

    calc.getFunction("is_prime").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(32237))
    )).value() shouldBe 1d

    calc.getFunction("is_prime").invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(32239))
    )).value() shouldBe 0d
  }
}
