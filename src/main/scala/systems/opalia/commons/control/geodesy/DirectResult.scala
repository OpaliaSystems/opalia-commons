package systems.opalia.commons.control.geodesy

import systems.opalia.commons.control.units._


trait DirectResult {

  val target: GeoLocation
  val finalBearing: Radian
}
