package systems.opalia.commons.control.geodesy

import systems.opalia.commons.control.units._


trait InverseResult {

  val distance: Meter
  val initialBearing: Radian
  val finalBearing: Radian
}
