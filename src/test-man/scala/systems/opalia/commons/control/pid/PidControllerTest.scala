package systems.opalia.commons.control.pid

import java.awt.EventQueue
import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
import org.scalatest.flatspec._
import org.scalatest.matchers.should._
import systems.opalia.commons.scripting.calculator.{Calculator, FunctionApp}
import systems.opalia.commons.scripting.js.JsScriptService
import systems.opalia.interfaces.scripting.ScriptEngine


class PidControllerTest
  extends AnyFlatSpec
    with Matchers {

  val scriptEngine: ScriptEngine =
    (new JsScriptService()).newScriptEngine()

  it should "be possible to get acceptable PID controller results with manual tests" in {

    val scheduler: ScheduledExecutorService =
      Executors.newScheduledThreadPool(10)

    scriptEngine.withSession {
      session =>

        val calculator = new Calculator(session)

        calculator.bindDefaultFunctions()

        val pid =
          new PidController(
            PidController.Tuning(
              proportionalGain = 0,
              integralGain = 0,
              derivativeGain = 0,
              filterCoefficient = 0,
              proportionalOnMeasurement = false,
              inverse = false),
            PidController.OutputRange(2, -2))

        var plottingMethod: (Long, Double, Double, Double) => Unit =
          (_, _, _, _) => {
          }

        val signalHandler: SignalHandler =
          new SignalHandler(scheduler, pid, (time, setpoint, input, output) => {

            plottingMethod(time, setpoint, input, output)
          })

        val config =
          new Configuration {

            def setGeneratorFunction(app: FunctionApp): Unit = {

              signalHandler.setGeneratorFunction(app)
            }

            def setFeedbackFunction(app: FunctionApp): Unit = {

              signalHandler.setFeedbackFunction(app)
            }

            def startSignalCalculation(calculationSkip: Long,
                                       sampleTime: Long,
                                       offset: Double,
                                       factor: Double): Unit = {

              signalHandler.start(calculationSkip, sampleTime, offset, factor)
            }

            def stopSignalCalculation(): Unit = {

              signalHandler.stop()
            }

            def setPidTuning(tuning: PidController.Tuning): Unit = {

              pid.setTuning(tuning)
            }

            def getPidTuning: PidController.Tuning = {

              pid.getTuning
            }

            def setPidOutputRange(range: PidController.OutputRange): Unit = {

              pid.setOutputRange(range)
            }

            def getPidOutputRange: PidController.OutputRange = {

              pid.getOutputRange
            }

            def runPid(run: Boolean): Unit = {

              if (run)
                pid.on()
              else
                pid.off()
            }

            def resetPid(): Unit = {

              pid.reset()
            }

            def initializePid(): Unit = {

              pid.initialize()
            }

            def setPlottingMethod(f: (Long, Double, Double, Double) => Unit): Unit = {

              plottingMethod = f
            }

            val generatorScripts: java.util.Map[String, String] = {

              val scripts = new java.util.LinkedHashMap[String, String]

              scripts.put("No Signal",
                """
                  |0
                  |
                """.stripMargin.trim)

              scripts.put("Step Signal",
                """
                  |(n . (x . if (x < n) 0 1) (Sample % (n * 2))) 50
                  |
                """.stripMargin.trim)

              scripts.put("Rectangular Pulse Signal",
                """
                  |((f x) a b . if ((abs (f a)) > b) 1 0)
                  |(x . sin ((Time / SampleTime) * π * x))
                  |0.02
                  |0.95
                  |
                """.stripMargin.trim)

              scripts.put("Sinc Pulse Signal",
                """
                  |(x . if (x = 0) 1 ((sin (π * x)) / (π * x)))
                  |(((Time / SampleTime) / 8) % 19 - 9.5)
                  |
                """.stripMargin.trim)

              scripts.put("Gaussian Pulse Signal",
                """
                  |(x μ σ (p σ) . (p σ) * (1 / (σ * sqrt (π * 2))) * exp (-((x - μ)^2) / (σ^2 * 2)))
                  |(((Time / SampleTime) / 5) % 10)
                  |5
                  |0.98
                  |(σ . σ * sqrt (π * 2))
                  |
                """.stripMargin.trim)

              scripts.put("Bump Signal",
                """
                  |((f x) a b . (y . if ((abs y) > b) (sig y) 0) (f a))
                  |(x . sin ((Time / SampleTime) * π * x))
                  |0.02
                  |0.95
                  |
                """.stripMargin.trim)

              scripts.put("Doublet Signal",
                """
                  |((f x) a b . (y . if ((cos y) < -b) (sig (sin y)) 0) (f a))
                  |(x . (Time / SampleTime) * π * x)
                  |0.02
                  |0.15
                  |
                """.stripMargin.trim)

              scripts.put("Sinus Signal",
                """
                  |sin ((Time / SampleTime) * π * 0.04)
                  |
                """.stripMargin.trim)

              scripts
            }

            val feedbackScripts: java.util.Map[String, String] = {

              val scripts = new java.util.LinkedHashMap[String, String]

              scripts.put("Simple Sum",
                """
                  |Input + Output
                  |
                """.stripMargin.trim)

              scripts.put("Simple Sum (inverse)",
                """
                  |Input - Output
                  |
                """.stripMargin.trim)

              scripts
            }
          }

        EventQueue.invokeLater(() => {
          new Dialog(scheduler, config, calculator)
        })

        scheduler.awaitTermination(Long.MaxValue, TimeUnit.MILLISECONDS)
    }
  }
}
