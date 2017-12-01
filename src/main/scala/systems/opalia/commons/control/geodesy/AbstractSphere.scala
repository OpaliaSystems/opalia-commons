package systems.opalia.commons.control.geodesy

import systems.opalia.commons.control.units._
import systems.opalia.commons.number._


abstract class AbstractSphere
  extends Ellipsoid {

  // https://en.wikipedia.org/wiki/Haversine_formula

  val radiusEquatorial = 6371000.800
  val radiusPolar = radiusEquatorial

  def solveInverseProblem(origin: GeoLocation, target: GeoLocation): InverseResult =
    new InverseResult {

      val distance = calculateDistance(origin, target)
      val initialBearing = calculateInitialBearing(origin, target)
      val finalBearing = calculateFinalBearing(origin, target)
    }

  def solveDirectProblem(origin: GeoLocation, bearing: Radian, distance: Meter): DirectResult = {

    val targetLocation = calculateTarget(origin, bearing, distance)

    new DirectResult {

      val target = targetLocation
      val finalBearing = calculateFinalBearing(origin, targetLocation)
    }
  }

  def calculateTarget(origin: GeoLocation, bearing: Radian, distance: Meter): GeoLocation = {

    val r = radiusEquatorial

    val φ1 = origin.latitude
    val λ1 = origin.longitude

    val φ2 = math.asin(math.sin(φ1) * math.cos(distance / r) +
      math.cos(φ1) * math.sin(distance / r) * math.cos(bearing))

    val λ2 = λ1 + math.atan2(math.sin(bearing) * math.sin(distance / r) * math.cos(φ1),
      math.cos(distance / r) - math.sin(φ1) * math.sin(φ2))

    GeoLocation(mathx.normalizeAngle180(φ2), mathx.normalizeAngle180(λ2))
  }

  def calculateDistance(origin: GeoLocation, target: GeoLocation): Meter = {

    val r = radiusEquatorial

    val φ1 = origin.latitude
    val φ2 = target.latitude
    val λ1 = origin.longitude
    val λ2 = target.longitude

    val Δφ = φ2 - φ1
    val Δλ = λ2 - λ1

    val a = math.sin(Δφ / 2) * math.sin(Δφ / 2) + math.cos(φ1) * math.cos(φ2) * math.sin(Δλ / 2) * math.sin(Δλ / 2)
    val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))

    r * c
  }

  def calculateInitialBearing(origin: GeoLocation, target: GeoLocation): Radian = {

    val φ1 = origin.latitude
    val φ2 = target.latitude
    val λ1 = origin.longitude
    val λ2 = target.longitude

    val Δλ = λ2 - λ1

    val y = math.cos(φ2) * math.sin(Δλ)
    val x = math.cos(φ1) * math.sin(φ2) - math.sin(φ1) * math.cos(φ2) * math.cos(Δλ)

    math.atan2(y, x)
  }

  def calculateFinalBearing(origin: GeoLocation, target: GeoLocation): Radian = {

    val φ1 = origin.latitude
    val φ2 = target.latitude
    val λ1 = origin.longitude
    val λ2 = target.longitude

    val Δλ = λ2 - λ1

    val y = math.cos(φ1) * math.sin(Δλ)
    val x = -(math.cos(φ2) * math.sin(φ1)) + math.sin(φ2) * math.cos(φ1) * math.cos(Δλ)

    math.atan2(y, x)
  }
}
