package systems.opalia.commons.control.geodesy

import systems.opalia.commons.control.units._


abstract class AbstractWGS84
  extends Ellipsoid {

  // https://en.wikipedia.org/wiki/Vincenty%27s_formulae
  // http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf

  val radiusEquatorial = 6378137.000
  val radiusPolar = 6356752.314

  def solveInverseProblem(origin: GeoLocation, target: GeoLocation): InverseResult = {

    val φ1 = origin.latitude
    val φ2 = target.latitude
    val λ1 = origin.longitude
    val λ2 = target.longitude

    val Δλ0 = λ2 - λ1
    val φ1U = math.atan((1.0 - flattening) * math.tan(φ1))
    val φ2U = math.atan((1.0 - flattening) * math.tan(φ2))

    val sinφ1U = math.sin(φ1U)
    val cosφ1U = math.cos(φ1U)
    val sinφ2U = math.sin(φ2U)
    val cosφ2U = math.cos(φ2U)

    var iterations = 20

    var Δλ = Δλ0
    var sinΔλ = 0.0
    var cosΔλ = 0.0
    var σ = 0.0
    var Δσ = 0.0
    var sinσ = 0.0
    var cosσ = 0.0

    var abc = (0.0, 0.0, 0.0)

    do {

      sinΔλ = math.sin(Δλ)
      cosΔλ = math.cos(Δλ)

      val t1 = cosφ2U * sinΔλ
      val t2 = cosφ1U * sinφ2U - sinφ1U * cosφ2U * cosΔλ

      sinσ = math.sqrt(t1 * t1 + t2 * t2)
      cosσ = sinφ1U * sinφ2U + cosφ1U * cosφ2U * cosΔλ

      σ = math.atan2(sinσ, cosσ)

      val sinα = if (sinσ == 0) 0.0 else cosφ1U * cosφ2U * sinΔλ / sinσ

      val cosSqα = 1.0 - sinα * sinα

      val cos2σM = if (cosSqα == 0) 0.0 else cosσ - 2.0 * sinφ1U * sinφ2U / cosSqα

      abc = calcABC(cosSqα)

      Δσ = abc._2 * sinσ * (cos2σM + (abc._2 / 4.0) * (cosσ * (-1.0 + 2.0 * cos2σM * cos2σM) - (abc._2 / 6.0) *
        cos2σM * (-3.0 + 4.0 * sinσ * sinσ) * (-3.0 + 4.0 * cos2σM * cos2σM)))

      val Δλx = Δλ

      Δλ = Δλ0 + (1.0 - abc._3) * flattening * sinα * (σ + abc._3 * sinσ * (cos2σM + abc._3 * cosσ *
        (-1.0 + 2.0 * cos2σM * cos2σM)))

      if (math.abs((Δλ - Δλx) / Δλ) < 1.0e-12)
        iterations = 0
      else
        iterations -= 1

    } while (iterations > 0)

    new InverseResult {

      val distance = radiusPolar * abc._1 * (σ - Δσ)
      val initialBearing = math.atan2(cosφ2U * sinΔλ, cosφ1U * sinφ2U - sinφ1U * cosφ2U * cosΔλ)
      val finalBearing = math.atan2(cosφ1U * sinΔλ, -(cosφ2U * sinφ1U) + sinφ2U * cosφ1U * cosΔλ)
    }
  }

  def solveDirectProblem(origin: GeoLocation, bearing: Radian, distance: Meter): DirectResult = {

    val φ1 = origin.latitude
    val λ1 = origin.longitude

    val sinα1 = math.sin(bearing)
    val cosα1 = math.cos(bearing)

    val tanφ1U = (1.0 - flattening) * math.tan(φ1)
    val cosφ1U = 1.0 / math.sqrt(1.0 + tanφ1U * tanφ1U)
    val sinφ1U = tanφ1U * cosφ1U

    val σ1 = math.atan2(tanφ1U, cosα1)

    val sinα = cosφ1U * sinα1

    val cosSqα = 1.0 - sinα * sinα

    val abc = calcABC(cosSqα)

    var iterations = 20

    var σ = distance / (radiusPolar * abc._1)
    var σx = 0.0
    var sinσ = 0.0
    var cosσ = 0.0
    var cos2σM = 0.0

    do {

      cos2σM = math.cos(2.0 * σ1 + σ)

      sinσ = math.sin(σ)
      cosσ = math.cos(σ)

      val Δσ = abc._2 * sinσ * (cos2σM + abc._2 / 4.0 * (cosσ * (-1.0 + 2.0 * cos2σM * cos2σM) -
        abc._2 / 6.0 * cos2σM * (-3.0 + 4.0 * sinσ * sinσ) * (-3.0 + 4.0 * cos2σM * cos2σM)))

      σx = σ
      σ = distance / (radiusPolar * abc._1) + Δσ

      if (math.abs(σ - σx) < 1.0e-12)
        iterations = 0
      else
        iterations -= 1

    } while (iterations > 0)

    val t = sinφ1U * sinσ - cosφ1U * cosσ * cosα1

    val φ2 = math.atan2(sinφ1U * cosσ + cosφ1U * sinσ * cosα1, (1.0 - flattening) * math.sqrt(sinα * sinα + t * t))

    val λ = math.atan2(sinσ * sinα1, cosφ1U * cosσ - sinφ1U * sinσ * cosα1)

    val l = λ - (1.0 - abc._3) * flattening * sinα *
      (σ + abc._3 * sinσ * (cos2σM + abc._3 * cosσ * (-1.0 + 2.0 * cos2σM * cos2σM)))

    val λ2 = (λ1 + l + 3.0 * math.Pi) % (2.0 * math.Pi) - math.Pi

    new DirectResult {

      val target = GeoLocation(φ2, λ2)
      val finalBearing = math.atan2(sinα, -t)
    }
  }

  private def calcABC(cosSqα: Double): (Double, Double, Double) = {

    val rφ = radiusEquatorial
    val rλ = radiusPolar

    val uSq = cosSqα * ((rφ * rφ - rλ * rλ) / (rλ * rλ))

    val a = 1 + (uSq / 16384.0) * (4096.0 + uSq * (-768.0 + uSq * (320.0 - 175.0 * uSq)))
    val b = (uSq / 1024.0) * (256.0 + uSq * (-128.0 + uSq * (74.0 - 47.0 * uSq)))
    val c = (flattening / 16.0) * cosSqα * (4.0 + flattening * (4.0 - 3.0 * cosSqα))

    (a, b, c)
  }
}
