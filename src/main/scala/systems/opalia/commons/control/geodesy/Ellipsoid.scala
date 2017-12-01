package systems.opalia.commons.control.geodesy

import systems.opalia.commons.control.units._


trait Ellipsoid {

  val radiusEquatorial: Meter
  val radiusPolar: Meter
  lazy val flattening: Double = 1.0 - (radiusPolar / radiusEquatorial)

  def solveInverseProblem(origin: GeoLocation, target: GeoLocation): InverseResult

  def solveDirectProblem(origin: GeoLocation, bearing: Radian, distance: Meter): DirectResult
}

object Ellipsoid {

  object Sphere
    extends AbstractSphere

  object WGS84
    extends AbstractWGS84

}
