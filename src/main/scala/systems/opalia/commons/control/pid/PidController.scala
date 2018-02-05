package systems.opalia.commons.control.pid


class PidController(private var tuning: PidController.Tuning,
                    private var range: PidController.OutputRange) {

  // http://brettbeauregard.com/blog/2011/04/improving-the-beginners-pid-introduction/
  // https://github.com/br3ttb/Arduino-PID-Library/

  private var running: Boolean = false
  private var timeLast: Long = 0
  private var setpoint: Double = 0
  private var input, inputLast: Double = 0
  private var output, outputSum: Double = 0

  reset()

  def reset(): Unit =
    synchronized {

      timeLast = System.currentTimeMillis()

      inputLast = 0
      outputSum = 0
      output = 0

      outputSum = range.bind(outputSum)
      output = range.bind(output)
    }

  def initialize(): Unit =
    synchronized {

      timeLast = System.currentTimeMillis()

      inputLast = input
      outputSum = output
    }

  def compute(): Unit =
    synchronized {

      if (running) {

        val timeNow = System.currentTimeMillis()
        val timeDelta = if (timeNow - timeLast == 0) Double.MinPositiveValue else (timeNow - timeLast).toDouble / 1000d

        val setpointCurrent = setpoint
        val inputCurrent = input
        val outputLast = output

        val error = setpointCurrent - inputCurrent
        val inputDelta = (inputCurrent - inputLast) / timeDelta

        timeLast = timeNow
        inputLast = inputCurrent

        outputSum += tuning.ki * error * timeDelta

        if (tuning.proportionalOnMeasurement)
          outputSum -= tuning.kp * inputDelta

        outputSum = range.bind(outputSum)

        output = outputSum - tuning.kd * inputDelta

        if (!tuning.proportionalOnMeasurement)
          output += tuning.kp * error

        output = tuning.filterCoefficient * outputLast + (1d - tuning.filterCoefficient) * output

        output = range.bind(output)
      }
    }

  def on(): Unit =
    synchronized {

      running = true
    }

  def off(): Unit =
    synchronized {

      running = false
    }

  def isRunning: Boolean =
    synchronized {

      running
    }

  def getTuning: PidController.Tuning =
    synchronized {

      this.tuning
    }

  def setTuning(tuning: PidController.Tuning): Unit =
    synchronized {

      this.tuning = tuning
    }

  def getOutputRange: PidController.OutputRange =
    synchronized {

      range
    }

  def setOutputRange(range: PidController.OutputRange): Unit =
    synchronized {

      this.range = range

      outputSum = range.bind(outputSum)
      output = range.bind(output)
    }

  def getSetpoint: Double =
    synchronized {

      this.setpoint
    }

  def setSetpoint(setpoint: Double): Unit =
    synchronized {

      this.setpoint = setpoint
    }

  def getInput: Double =
    synchronized {

      this.input
    }

  def setInput(input: Double): Unit =
    synchronized {

      this.input = input
    }

  def getOutput: Double =
    synchronized {

      this.output
    }
}

object PidController {

  case class Tuning(proportionalGain: Double,
                    integralGain: Double,
                    derivativeGain: Double,
                    filterCoefficient: Double,
                    proportionalOnMeasurement: Boolean,
                    inverse: Boolean) {

    if (proportionalGain < 0 || integralGain < 0 || derivativeGain < 0)
      throw new IllegalArgumentException("Expect positive values for every gain.")

    if (filterCoefficient < 0 || filterCoefficient > 1)
      throw new IllegalArgumentException("Expect value in interval [0,1] for filter coefficient.")

    val kp: Double = if (inverse) -proportionalGain else proportionalGain
    val ki: Double = if (inverse) -integralGain else integralGain
    val kd: Double = if (inverse) -derivativeGain else derivativeGain

    def invert: Tuning =
      Tuning(proportionalGain, integralGain, derivativeGain, filterCoefficient, proportionalOnMeasurement, !inverse)
  }

  case class OutputRange(maximum: Double, minimum: Double) {

    if (maximum <= minimum)
      throw new IllegalArgumentException("Expect range with maximum > minimum.")

    def bind(value: Double): Double =
      math.max(math.min(value, maximum), minimum)
  }

}
