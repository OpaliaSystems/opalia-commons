package systems.opalia.commons.control.pid

import java.time.Instant
import java.util.concurrent.{ScheduledExecutorService, ScheduledFuture, TimeUnit}
import systems.opalia.commons.scripting.calculator.FunctionApp


class SignalHandler(scheduler: ScheduledExecutorService,
                    pid: PidController,
                    fetch: (Long, Double, Double, Double) => Unit) {

  private var sample: Long = 0
  private var startTime: Long = 0
  private var lastTime: Long = 0
  private var cancellable: ScheduledFuture[_] = _
  private var generatorFunction: FunctionApp = _
  private var feedbackFunction: FunctionApp = _

  def start(calculationSkip: Long,
            sampleTime: Long,
            offset: Double,
            factor: Double): Unit = {

    if (!isRunning) {

      this.sample = 0
      this.startTime = Instant.now.toEpochMilli
      this.lastTime = 0

      pid.reset()
      pid.setInput(0)

      cancellable = scheduler.scheduleAtFixedRate(
        () => {

          val currentTime = Instant.now.toEpochMilli - startTime
          val deltaTime = if (lastTime == 0) 0 else currentTime - lastTime

          val amplitude =
            applyGeneratorFunction(
              sampleTime,
              sample,
              currentTime,
              deltaTime)

          val setpoint =
            offset + amplitude * factor

          pid.setSetpoint(setpoint)

          if (sample % (calculationSkip + 1) == 0) {

            val feedback =
              applyFeedbackFunction(
                sampleTime,
                sample,
                currentTime,
                deltaTime,
                setpoint,
                pid.getInput,
                pid.getOutput)

            pid.setInput(feedback)

            pid.compute()
          }

          fetch(currentTime, setpoint, pid.getInput, pid.getOutput)

          sample += 1
          lastTime = currentTime
        },
        0,
        sampleTime,
        TimeUnit.MILLISECONDS)
    }
  }

  def stop(): Unit = {

    if (isRunning) {

      cancellable.cancel(false)
      cancellable = null
    }
  }

  def isRunning: Boolean =
    cancellable != null

  def getGeneratorFunction: FunctionApp =
    generatorFunction

  def setGeneratorFunction(generatorFunction: FunctionApp): Unit =
    this.generatorFunction = generatorFunction

  def getFeedbackFunction: FunctionApp =
    feedbackFunction

  def setFeedbackFunction(feedbackFunction: FunctionApp): Unit =
    this.feedbackFunction = feedbackFunction

  private def applyGeneratorFunction(sampleTime: Long,
                                     sample: Long,
                                     currentTime: Long,
                                     deltaTime: Long): Double = {

    if (generatorFunction == null)
      throw new IllegalStateException("Signal function is not initialized.")

    generatorFunction.invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(sampleTime)),
      FunctionApp.wrap(FunctionApp.fromDouble(sample)),
      FunctionApp.wrap(FunctionApp.fromDouble(currentTime)),
      FunctionApp.wrap(FunctionApp.fromDouble(deltaTime))
    )).value()
  }

  private def applyFeedbackFunction(sampleTime: Long,
                                    sample: Long,
                                    currentTime: Long,
                                    deltaTime: Long,
                                    setpoint: Double,
                                    input: Double,
                                    output: Double): Double = {

    if (feedbackFunction == null)
      throw new IllegalStateException("Feedback function is not initialized.")

    feedbackFunction.invoke(Vector(
      FunctionApp.wrap(FunctionApp.fromDouble(sampleTime)),
      FunctionApp.wrap(FunctionApp.fromDouble(sample)),
      FunctionApp.wrap(FunctionApp.fromDouble(currentTime)),
      FunctionApp.wrap(FunctionApp.fromDouble(deltaTime)),
      FunctionApp.wrap(FunctionApp.fromDouble(setpoint)),
      FunctionApp.wrap(FunctionApp.fromDouble(input)),
      FunctionApp.wrap(FunctionApp.fromDouble(output))
    )).value()
  }
}
