package systems.opalia.commons.control.units

case class LinearVelocity(x: MeterPerSecond,
                          y: MeterPerSecond,
                          z: MeterPerSecond) {

  def sum: MeterPerSecond =
    math.abs(x) + math.abs(y) + math.abs(z)
}
