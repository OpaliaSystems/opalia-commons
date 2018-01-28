package systems.opalia.commons.scripting.calculator


abstract class CalculatorException(message: String)
  extends Exception(message)

class CalculatorFormatException(message: String)
  extends CalculatorException(message)

class CalculatorParserException(message: String)
  extends CalculatorException(message)

class CalculatorRuntimeException(message: String)
  extends CalculatorException(message)
