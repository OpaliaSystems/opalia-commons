package systems.opalia.commons.control.units

case class AngularAcceleration(x: RadianPerSecondSq,
                               y: RadianPerSecondSq,
                               z: RadianPerSecondSq) {

  def sum: RadianPerSecondSq =
    math.abs(x) + math.abs(y) + math.abs(z)
}
