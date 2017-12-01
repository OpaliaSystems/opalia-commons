package systems.opalia.commons.control.units

case class AngularVelocity(x: RadianPerSecond,
                           y: RadianPerSecond,
                           z: RadianPerSecond) {

  def sum: RadianPerSecond =
    math.abs(x) + math.abs(y) + math.abs(z)
}
