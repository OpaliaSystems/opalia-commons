package systems.opalia.commons.control.units

case class LinearAcceleration(x: MeterPerSecondSq,
                              y: MeterPerSecondSq,
                              z: MeterPerSecondSq) {

  def sum: MeterPerSecondSq =
    math.abs(x) + math.abs(y) + math.abs(z)
}
