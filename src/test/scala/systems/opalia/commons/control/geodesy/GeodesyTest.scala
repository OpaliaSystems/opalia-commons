package systems.opalia.commons.control.geodesy

import org.scalatest._
import systems.opalia.commons.control.units._
import systems.opalia.commons.number._


class GeodesyTest
  extends FlatSpec
    with Matchers {

  val pDresden = GeoLocation(51.049259.toRadians, 13.73836.toRadians)
  val pTokyo = GeoLocation(35.683889.toRadians, 139.774444.toRadians)
  val pSanFrancisco = GeoLocation(37.7793.toRadians, -122.4192.toRadians)
  val pMelbourne = GeoLocation(-37.8.toRadians, 144.95.toRadians)
  val pAlagoinhas = GeoLocation(-12.125458.toRadians, -38.411036.toRadians)

  it should "solve the inverse problem with calculation based on a sphere" in {

    val _01 = Ellipsoid.Sphere.solveInverseProblem(pDresden, pTokyo)

    _01.distance should be(9049758.598 +- 30000.0)
    _01.initialBearing.toDegrees should be(41.6 +- 1.5)
    _01.finalBearing.toDegrees should be(149.0 +- 1.5)

    val _02 = Ellipsoid.Sphere.solveInverseProblem(pTokyo, pSanFrancisco)

    _02.distance should be(8283557.164 +- 30000.0)
    _02.initialBearing.toDegrees should be(54.4 +- 1.5)
    _02.finalBearing.toDegrees should be(123.3 +- 1.5)

    val _03 = Ellipsoid.Sphere.solveInverseProblem(pSanFrancisco, pAlagoinhas)

    _03.distance should be(10310512.832 +- 30000.0)
    _03.initialBearing.toDegrees should be(103.1 +- 1.5)
    _03.finalBearing.toDegrees should be(128.0 +- 1.5)

    val _04 = Ellipsoid.Sphere.solveInverseProblem(pAlagoinhas, pMelbourne)

    _04.distance should be(14466649.165 +- 30000.0)
    _04.initialBearing.toDegrees should be(-176.5 +- 1.5)
    _04.finalBearing.toDegrees should be(-4.3 +- 1.5)

    val _05 = Ellipsoid.Sphere.solveInverseProblem(pMelbourne, pDresden)

    _05.distance should be(15949946.036 +- 30000.0)
    _05.initialBearing.toDegrees should be(-52.7 +- 1.5)
    _05.finalBearing.toDegrees should be(-92.1 +- 1.5)

    val _06 = Ellipsoid.Sphere.solveInverseProblem(pTokyo, pAlagoinhas)

    _06.distance should be(17388171.334 +- 30000.0)
    _06.initialBearing.toDegrees should be(-4.4 +- 1.5)
    _06.finalBearing.toDegrees should be(-176.4 +- 1.5)

    val _07 = Ellipsoid.Sphere.solveInverseProblem(pSanFrancisco, pMelbourne)

    _07.distance should be(12645356.209 +- 30000.0)
    _07.initialBearing.toDegrees should be(-120.2 +- 1.5)
    _07.finalBearing.toDegrees should be(-120.2 +- 1.5)
  }

  it should "solve the inverse problem with calculation based on WGS84" in {

    val _01 = Ellipsoid.WGS84.solveInverseProblem(pDresden, pTokyo)

    mathx.round(_01.distance, 3) shouldBe 9049758.598
    mathx.round(_01.initialBearing.toDegrees, 1) shouldBe 41.6
    mathx.round(_01.finalBearing.toDegrees, 1) shouldBe 149.0

    val _02 = Ellipsoid.WGS84.solveInverseProblem(pTokyo, pSanFrancisco)

    mathx.round(_02.distance, 3) shouldBe 8283557.164
    mathx.round(_02.initialBearing.toDegrees, 1) shouldBe 54.4
    mathx.round(_02.finalBearing.toDegrees, 1) shouldBe 123.3

    val _03 = Ellipsoid.WGS84.solveInverseProblem(pSanFrancisco, pAlagoinhas)

    mathx.round(_03.distance, 3) shouldBe 10310512.832
    mathx.round(_03.initialBearing.toDegrees, 1) shouldBe 103.1
    mathx.round(_03.finalBearing.toDegrees, 1) shouldBe 128.0

    val _04 = Ellipsoid.WGS84.solveInverseProblem(pAlagoinhas, pMelbourne)

    mathx.round(_04.distance, 3) shouldBe 14466649.165
    mathx.round(_04.initialBearing.toDegrees, 1) shouldBe -176.5
    mathx.round(_04.finalBearing.toDegrees, 1) shouldBe -4.3

    val _05 = Ellipsoid.WGS84.solveInverseProblem(pMelbourne, pDresden)

    mathx.round(_05.distance, 3) shouldBe 15949946.036
    mathx.round(_05.initialBearing.toDegrees, 1) shouldBe -52.7
    mathx.round(_05.finalBearing.toDegrees, 1) shouldBe -92.1

    val _06 = Ellipsoid.WGS84.solveInverseProblem(pTokyo, pAlagoinhas)

    mathx.round(_06.distance, 3) shouldBe 17388171.334
    mathx.round(_06.initialBearing.toDegrees, 1) shouldBe -4.4
    mathx.round(_06.finalBearing.toDegrees, 1) shouldBe -176.4

    val _07 = Ellipsoid.WGS84.solveInverseProblem(pSanFrancisco, pMelbourne)

    mathx.round(_07.distance, 3) shouldBe 12645356.209
    mathx.round(_07.initialBearing.toDegrees, 1) shouldBe -120.2
    mathx.round(_07.finalBearing.toDegrees, 1) shouldBe -120.2
  }

  it should "solve the direct problem with calculation based on a sphere" in {

    solveBothProblems(Ellipsoid.Sphere, pDresden, pTokyo)
    solveBothProblems(Ellipsoid.Sphere, pTokyo, pSanFrancisco)
    solveBothProblems(Ellipsoid.Sphere, pSanFrancisco, pAlagoinhas)
    solveBothProblems(Ellipsoid.Sphere, pAlagoinhas, pMelbourne)
    solveBothProblems(Ellipsoid.Sphere, pMelbourne, pDresden)
    solveBothProblems(Ellipsoid.Sphere, pTokyo, pAlagoinhas)
    solveBothProblems(Ellipsoid.Sphere, pSanFrancisco, pMelbourne)
  }

  it should "solve the direct problem with calculation based on WGS84" in {

    solveBothProblems(Ellipsoid.WGS84, pDresden, pTokyo)
    solveBothProblems(Ellipsoid.WGS84, pTokyo, pSanFrancisco)
    solveBothProblems(Ellipsoid.WGS84, pSanFrancisco, pAlagoinhas)
    solveBothProblems(Ellipsoid.WGS84, pAlagoinhas, pMelbourne)
    solveBothProblems(Ellipsoid.WGS84, pMelbourne, pDresden)
    solveBothProblems(Ellipsoid.WGS84, pTokyo, pAlagoinhas)
    solveBothProblems(Ellipsoid.WGS84, pSanFrancisco, pMelbourne)
  }

  private def solveBothProblems(ellipsoid: Ellipsoid, a: GeoLocation, b: GeoLocation): Unit = {

    val res1 = ellipsoid.solveInverseProblem(a, b)
    val res2 = ellipsoid.solveDirectProblem(a, res1.initialBearing, res1.distance)

    val invFinalBearing = mathx.normalizeAngle180(res1.finalBearing + math.Pi)
    val invInitialBearing = mathx.normalizeAngle180(res1.initialBearing + math.Pi)

    val res3 = ellipsoid.solveDirectProblem(b, invFinalBearing, res1.distance)

    mathx.round(res2.target.latitude.toDegrees, 7) shouldBe mathx.round(b.latitude.toDegrees, 7)
    mathx.round(res2.target.longitude.toDegrees, 7) shouldBe mathx.round(b.longitude.toDegrees, 7)
    mathx.round(res2.finalBearing.toDegrees, 7) shouldBe mathx.round(res1.finalBearing.toDegrees, 7)

    mathx.round(res3.target.latitude.toDegrees, 7) shouldBe mathx.round(a.latitude.toDegrees, 7)
    mathx.round(res3.target.longitude.toDegrees, 7) shouldBe mathx.round(a.longitude.toDegrees, 7)
    mathx.round(res3.finalBearing.toDegrees, 7) shouldBe mathx.round(invInitialBearing.toDegrees, 7)
  }
}
