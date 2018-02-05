package systems.opalia.commons.control.pid

import java.util.function.Consumer
import systems.opalia.commons.scripting.calculator.{FunctionApp, FunctionDef}


trait Configuration {

  val generatorSignature: FunctionDef.Signature =
    FunctionDef.Signature("", List(
      FunctionDef.Signature("SampleTime", Nil, None),
      FunctionDef.Signature("Sample", Nil, None),
      FunctionDef.Signature("Time", Nil, None),
      FunctionDef.Signature("ΔTime", Nil, None),
    ), None)

  val feedbackSignature: FunctionDef.Signature =
    FunctionDef.Signature("", List(
      FunctionDef.Signature("SampleTime", Nil, None),
      FunctionDef.Signature("Sample", Nil, None),
      FunctionDef.Signature("Time", Nil, None),
      FunctionDef.Signature("ΔTime", Nil, None),
      FunctionDef.Signature("Setpoint", Nil, None),
      FunctionDef.Signature("Input", Nil, None),
      FunctionDef.Signature("Output", Nil, None),
    ), None)

  val generatorScripts: java.util.Map[String, String]

  val feedbackScripts: java.util.Map[String, String]

  def setGeneratorFunction(app: FunctionApp): Unit

  def setFeedbackFunction(app: FunctionApp): Unit

  def startSignalCalculation(calculationSkip: Long,
                             sampleTime: Long,
                             offset: Double,
                             factor: Double): Unit

  def stopSignalCalculation(): Unit

  def setPidTuning(tuning: PidController.Tuning): Unit

  def getPidTuning: PidController.Tuning

  def setPidOutputRange(range: PidController.OutputRange): Unit

  def getPidOutputRange: PidController.OutputRange

  def runPid(run: Boolean): Unit

  def resetPid(): Unit

  def initializePid(): Unit

  def setPlottingMethod(f: Consumer[(java.lang.Long, java.lang.Double, java.lang.Double, java.lang.Double)]): Unit =
    setPlottingMethod((time, setpoint, input, output) => f.accept((time, setpoint, input, output)))

  def setPlottingMethod(f: (Long, Double, Double, Double) => Unit): Unit
}
