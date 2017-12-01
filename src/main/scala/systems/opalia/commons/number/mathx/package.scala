package systems.opalia.commons.number


package object mathx {

  def log(value: Double, base: Double): Double =
    math.log(value) / math.log(base)

  def round(value: Double, pos: Int): Double = {

    val factor = math.pow(10d, pos)

    math.round(value * factor) / factor
  }

  def floor(value: Double, pos: Int): Double = {

    val factor = math.pow(10d, pos)

    math.floor(value * factor) / factor
  }

  def ceil(value: Double, pos: Int): Double = {

    val factor = math.pow(10d, pos)

    math.ceil(value * factor) / factor
  }

  // normalize percent in interval [0, 1]
  def normalizePercent(value: Double): Double =
    math.max(0d, math.min(1d, value))

  // normalize angle in interval [0,2π]
  def normalizeAngle360(value: Double): Double = {

    val angle = value % (2d * math.Pi)

    if (angle < 0d)
      angle + (2d * math.Pi)
    else
      angle
  }

  // normalize angle in interval [-π,+π]
  def normalizeAngle180(value: Double): Double = {

    val angle = value % (2d * math.Pi)

    if (angle > math.Pi)
      angle - 2d * math.Pi
    else if (angle < -math.Pi)
      angle + 2d * math.Pi
    else
      angle
  }

  // normalize angle in interval [-π/2,+π/2]
  def normalizeAngle090(value: Double): Double = {

    val x = if (math.abs(value) % (2d * math.Pi) > math.Pi) -1d else 1d
    val angle = value % math.Pi

    if (angle > math.Pi / 2d)
      (math.Pi - angle) * x
    else if (angle < -math.Pi / 2d)
      (-math.Pi - angle) * x
    else
      angle * x
  }
}
