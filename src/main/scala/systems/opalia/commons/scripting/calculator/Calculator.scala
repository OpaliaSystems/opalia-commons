package systems.opalia.commons.scripting.calculator

import jdk.nashorn.api.scripting.ScriptObjectMirror
import scala.collection.mutable
import systems.opalia.commons.number.mathx
import systems.opalia.commons.scripting.JavaScript


class Calculator(js: JavaScript,
                 log: (String) => Unit = (_) => {}) {

  protected val context: JavaScript.Context = js.newContext()
  protected val functions: mutable.HashSet[FunctionDef] = mutable.HashSet[FunctionDef]()
  protected val operators: mutable.HashSet[FunctionDef.OperatorEntry] = mutable.HashSet[FunctionDef.OperatorEntry]()

  protected val parser = new CalculatorParser()
  protected val compilerFactory = CompilerFactory.newCompilerFactory(CompilerFactory.CompilerType.JavaScript)

  def getFunction(descriptor: String): FunctionApp = {

    val function =
      functions.find(_.signature.descriptor == descriptor)
        .getOrElse(throw new CalculatorRuntimeException(s"Cannot find function with descriptor $descriptor."))

    val mirror =
      context.get(compilerFactory.encodeName(function.signature.descriptor)).get.asInstanceOf[ScriptObjectMirror]

    FunctionApp.fromObjectMirror(function.signature, mirror)
  }

  def bindFunction(signature: FunctionDef.Signature, f: (Vector[FunctionApp]) => FunctionApp): FunctionApp = {

    if (!parser.validateDescriptor(signature.descriptor))
      throw new CalculatorFormatException(s"Invalid descriptor ${signature.descriptor} not allowed.")

    if (functions.exists(_.signature.descriptor == signature.descriptor))
      throw new CalculatorRuntimeException(s"Duplication with function ${signature.descriptor} not allowed.")

    val function =
      FunctionDef.root(signature, () => functions.toSet, () => operators.toSet)

    val app =
      FunctionApp.fromFunction(function.signature, f)

    context.put(compilerFactory.encodeName(function.signature.descriptor), app)

    functions += function

    app
  }

  def bindFunction(source: String, signature: FunctionDef.Signature): FunctionApp = {

    processSource(
      (addFunction: (FunctionDef) => Unit,
       addOperator: (FunctionDef.OperatorEntry) => Unit,
       getGlobalFunctions: () => Set[FunctionDef],
       getGlobalOperators: () => Set[FunctionDef.OperatorEntry]) => {

        parser.parseBody(source, signature, addFunction, getGlobalFunctions, getGlobalOperators) :: Nil

      }).head
  }

  def bindFunctions(source: String): List[FunctionApp] = {

    processSource(
      (addFunction: (FunctionDef) => Unit,
       addOperator: (FunctionDef.OperatorEntry) => Unit,
       getGlobalFunctions: () => Set[FunctionDef],
       getGlobalOperators: () => Set[FunctionDef.OperatorEntry]) => {

        parser.parse(source, addFunction, addOperator, getGlobalFunctions, getGlobalOperators)
      })
  }

  def eval(source: String, signature: FunctionDef.Signature = FunctionDef.primitiveSignature): FunctionApp = {

    bindFunction(source, signature).invoke()
  }

  private def processSource(parse: (
    (FunctionDef) => Unit,
      (FunctionDef.OperatorEntry) => Unit,
      () => Set[FunctionDef],
      () => Set[FunctionDef.OperatorEntry]) => List[Ast.Body]): List[FunctionApp] = {

    val newFunctions =
      mutable.HashSet[FunctionDef]()

    val newOperators =
      mutable.HashSet[FunctionDef.OperatorEntry]()

    var getFunctions: () => Set[FunctionDef] =
      () => functions.toSet ++ newFunctions.toSet

    var getOperators: () => Set[FunctionDef.OperatorEntry] =
      () => operators.toSet ++ newOperators.toSet

    val getGlobalFunctions: () => Set[FunctionDef] =
      () => getFunctions()

    val getGlobalOperators: () => Set[FunctionDef.OperatorEntry] =
      () => getOperators()

    val addFunction: (FunctionDef) => Unit =
      (function: FunctionDef) => {

        if (function.signature.descriptor.nonEmpty) {

          if (getFunctions().exists(_.signature.descriptor == function.signature.descriptor))
            throw new CalculatorRuntimeException(
              s"Duplication with function ${function.signature.descriptor} not allowed.")

          newFunctions += function
        }
      }

    val addOperator: (FunctionDef.OperatorEntry) => Unit =
      (operator: FunctionDef.OperatorEntry) => {

        val function =
          getFunctions()
            .find(_.signature.descriptor == operator.descriptor)
            .getOrElse(throw new CalculatorParserException(
              s"Operator {${operator.operator}} refers to a non existing function ${operator.descriptor}."))

        if (function.signature.parameters.length > 2)
          throw new CalculatorParserException(
            s"Operator {${operator.operator}} is not applicable to function ${operator.descriptor}.")

        if (getOperators()
          .filter(_.operator == operator.operator)
          .flatMap(x => getFunctions().find(_.signature.descriptor == x.descriptor))
          .exists(_.signature.parameters.length == function.signature.parameters.length))
          throw new CalculatorParserException(
            s"Operator {${operator.operator}} is already defined to a function with the same number of parameters.")

        newOperators += operator
      }

    val apps =
      parse(addFunction, addOperator, getGlobalFunctions, getGlobalOperators).map {
        ast =>

          val result =
            compilerFactory.newCompiler(ast).toString

          log(result)

          val mirror =
            context.eval(result).asInstanceOf[ScriptObjectMirror]

          FunctionApp.fromObjectMirror(ast.function.signature, mirror)
      }

    functions ++= newFunctions
    operators ++= newOperators

    getFunctions =
      () => functions.toSet

    getOperators =
      () => operators.toSet

    apps
  }

  def bindDefaultFunctions(): Unit = {

    def simpleSignature(descriptor: String, length: Int): FunctionDef.Signature =
      FunctionDef.Signature(
        descriptor,
        (for (i <- 1 to length) yield FunctionDef.Signature(s"f$i", Nil, None)).toList,
        None)

    bindFunction(simpleSignature("if", 3), fs => {

      if (fs(0).invoke().value() != 0d) fs(1).invoke() else fs(2).invoke()
    })

    bindFunction(simpleSignature("E", 0), fs => {

      FunctionApp.fromDouble(math.E)
    })

    bindFunction(simpleSignature("π", 0), fs => {

      FunctionApp.fromDouble(math.Pi)
    })

    bindFunction(simpleSignature("NaN", 0), fs => {

      FunctionApp.fromDouble(Double.NaN)
    })

    bindFunction(simpleSignature("MaxN", 0), fs => {

      FunctionApp.fromDouble(Double.MaxValue)
    })

    bindFunction(simpleSignature("MinN", 0), fs => {

      FunctionApp.fromDouble(Double.MinValue)
    })

    bindFunction(simpleSignature("MinPosN", 0), fs => {

      FunctionApp.fromDouble(Double.MinPositiveValue)
    })

    bindFunction(simpleSignature("InfPosN", 0), fs => {

      FunctionApp.fromDouble(Double.PositiveInfinity)
    })

    bindFunction(simpleSignature("InfNegN", 0), fs => {

      FunctionApp.fromDouble(Double.NegativeInfinity)
    })

    bindFunction(simpleSignature("isNaN", 1), fs => {

      FunctionApp.fromDouble(if (fs(0).invoke().value().isNaN) 1d else 0d)
    })

    bindFunction(simpleSignature("isInf", 1), fs => {

      FunctionApp.fromDouble(if (fs(0).invoke().value().isInfinity) 1d else 0d)
    })

    bindFunction(simpleSignature("isInfPos", 1), fs => {

      FunctionApp.fromDouble(if (fs(0).invoke().value().isPosInfinity) 1d else 0d)
    })

    bindFunction(simpleSignature("isInfNeg", 1), fs => {

      FunctionApp.fromDouble(if (fs(0).invoke().value().isNegInfinity) 1d else 0d)
    })

    bindFunction(simpleSignature("isWhole", 1), fs => {

      FunctionApp.fromDouble(if (fs(0).invoke().value().isWhole()) 1d else 0d)
    })

    bindFunction(simpleSignature("rand", 0), fs => {

      FunctionApp.fromDouble(math.random())
    })

    bindFunction(simpleSignature("randr", 2), fs => {

      val (v1, v2) = (math.abs(fs(0).invoke().value()), math.abs(fs(1).invoke().value()))
      val (max, min) = (math.max(v1, v2), math.min(v1, v2))

      val result = min + (max - min) * math.random()

      FunctionApp.fromDouble(result)
    })

    bindFunction(simpleSignature("pos", 1), fs => {

      FunctionApp.fromDouble(+fs(0).invoke().value())
    })

    bindFunction(simpleSignature("neg", 1), fs => {

      FunctionApp.fromDouble(-fs(0).invoke().value())
    })

    bindFunction(simpleSignature("add", 2), fs => {

      FunctionApp.fromDouble(fs(0).invoke().value() + fs(1).invoke().value())
    })

    bindFunction(simpleSignature("sub", 2), fs => {

      FunctionApp.fromDouble(fs(0).invoke().value() - fs(1).invoke().value())
    })

    bindFunction(simpleSignature("mul", 2), fs => {

      FunctionApp.fromDouble(fs(0).invoke().value() * fs(1).invoke().value())
    })

    bindFunction(simpleSignature("div", 2), fs => {

      FunctionApp.fromDouble(fs(0).invoke().value() / fs(1).invoke().value())
    })

    bindFunction(simpleSignature("mod", 2), fs => {

      FunctionApp.fromDouble(fs(0).invoke().value() % fs(1).invoke().value())
    })

    bindFunction(simpleSignature("eq", 2), fs => {

      FunctionApp.fromDouble(if (fs(0).invoke().value() == fs(1).invoke().value()) 1d else 0d)
    })

    bindFunction(simpleSignature("ne", 2), fs => {

      FunctionApp.fromDouble(if (fs(0).invoke().value() != fs(1).invoke().value()) 1d else 0d)
    })

    bindFunction(simpleSignature("lt", 2), fs => {

      FunctionApp.fromDouble(if (fs(0).invoke().value() < fs(1).invoke().value()) 1d else 0d)
    })

    bindFunction(simpleSignature("le", 2), fs => {

      FunctionApp.fromDouble(if (fs(0).invoke().value() <= fs(1).invoke().value()) 1d else 0d)
    })

    bindFunction(simpleSignature("gt", 2), fs => {

      FunctionApp.fromDouble(if (fs(0).invoke().value() > fs(1).invoke().value()) 1d else 0d)
    })

    bindFunction(simpleSignature("ge", 2), fs => {

      FunctionApp.fromDouble(if (fs(0).invoke().value() >= fs(1).invoke().value()) 1d else 0d)
    })

    bindFunction(simpleSignature("and", 2), fs => {

      FunctionApp.fromDouble(if (fs(0).invoke().value() != 0d && fs(1).invoke().value() != 0d) 1d else 0d)
    })

    bindFunction(simpleSignature("or", 2), fs => {

      FunctionApp.fromDouble(if (fs(0).invoke().value() != 0d || fs(1).invoke().value() != 0d) 1d else 0d)
    })

    bindFunction(simpleSignature("xor", 2), fs => {

      val a = fs(0).invoke().value() != 0d
      val b = fs(1).invoke().value() != 0d

      FunctionApp.fromDouble(if (a && !b || !a && b) 1d else 0d)
    })

    bindFunction(simpleSignature("not", 1), fs => {

      FunctionApp.fromDouble(if (fs(0).invoke().value() == 0d) 1d else 0d)
    })

    bindFunction(simpleSignature("sin", 1), fs => {

      FunctionApp.fromDouble(math.sin(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("cos", 1), fs => {

      FunctionApp.fromDouble(math.cos(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("tan", 1), fs => {

      FunctionApp.fromDouble(math.tan(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("asin", 1), fs => {

      FunctionApp.fromDouble(math.asin(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("acos", 1), fs => {

      FunctionApp.fromDouble(math.acos(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("atan", 1), fs => {

      FunctionApp.fromDouble(math.atan(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("sinh", 1), fs => {

      FunctionApp.fromDouble(math.sinh(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("cosh", 1), fs => {

      FunctionApp.fromDouble(math.cosh(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("tanh", 1), fs => {

      FunctionApp.fromDouble(math.tanh(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("atan2", 2), fs => {

      FunctionApp.fromDouble(math.atan2(fs(0).invoke().value(), fs(1).invoke().value()))
    })

    bindFunction(simpleSignature("hypot", 2), fs => {

      FunctionApp.fromDouble(math.hypot(fs(0).invoke().value(), fs(1).invoke().value()))
    })

    bindFunction(simpleSignature("rad", 1), fs => {

      FunctionApp.fromDouble(math.toRadians(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("deg", 1), fs => {

      FunctionApp.fromDouble(math.toDegrees(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("sqrt", 1), fs => {

      FunctionApp.fromDouble(math.sqrt(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("cbrt", 1), fs => {

      FunctionApp.fromDouble(math.cbrt(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("pow", 2), fs => {

      FunctionApp.fromDouble(math.pow(fs(0).invoke().value(), fs(1).invoke().value()))
    })

    bindFunction(simpleSignature("exp", 1), fs => {

      FunctionApp.fromDouble(math.exp(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("expm1", 1), fs => {

      FunctionApp.fromDouble(math.expm1(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("ln", 1), fs => {

      FunctionApp.fromDouble(math.log(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("ln1p", 1), fs => {

      FunctionApp.fromDouble(math.log1p(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("log", 2), fs => {

      FunctionApp.fromDouble(mathx.log(fs(0).invoke().value(), fs(1).invoke().value()))
    })

    bindFunction(simpleSignature("log10", 1), fs => {

      FunctionApp.fromDouble(math.log10(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("ceil", 1), fs => {

      FunctionApp.fromDouble(math.ceil(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("ceiln", 2), fs => {

      FunctionApp.fromDouble(mathx.ceil(fs(0).invoke().value(), fs(1).invoke().value().toInt))
    })

    bindFunction(simpleSignature("floor", 1), fs => {

      FunctionApp.fromDouble(math.floor(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("floorn", 2), fs => {

      FunctionApp.fromDouble(mathx.floor(fs(0).invoke().value(), fs(1).invoke().value().toInt))
    })

    bindFunction(simpleSignature("round", 1), fs => {

      FunctionApp.fromDouble(math.round(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("roundn", 2), fs => {

      FunctionApp.fromDouble(mathx.round(fs(0).invoke().value(), fs(1).invoke().value().toInt))
    })

    bindFunction(simpleSignature("rint", 1), fs => {

      FunctionApp.fromDouble(math.rint(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("abs", 1), fs => {

      FunctionApp.fromDouble(math.abs(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("max", 2), fs => {

      FunctionApp.fromDouble(math.max(fs(0).invoke().value(), fs(1).invoke().value()))
    })

    bindFunction(simpleSignature("min", 2), fs => {

      FunctionApp.fromDouble(math.min(fs(0).invoke().value(), fs(1).invoke().value()))
    })

    bindFunction(simpleSignature("sig", 1), fs => {

      FunctionApp.fromDouble(math.signum(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("ulp", 1), fs => {

      FunctionApp.fromDouble(math.ulp(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("rem", 2), fs => {

      FunctionApp.fromDouble(math.IEEEremainder(fs(0).invoke().value(), fs(1).invoke().value()))
    })

    bindFunction(simpleSignature("normp", 1), fs => {

      FunctionApp.fromDouble(mathx.normalizePercent(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("norma360", 1), fs => {

      FunctionApp.fromDouble(mathx.normalizeAngle360(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("norma180", 1), fs => {

      FunctionApp.fromDouble(mathx.normalizeAngle180(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("norma090", 1), fs => {

      FunctionApp.fromDouble(mathx.normalizeAngle090(fs(0).invoke().value()))
    })

    bindFunction(simpleSignature("bit_sl", 2), fs => {

      FunctionApp.fromDouble(binaryHelperFunction(fs(0).invoke().value(), fs(1).invoke().value(), {
        (a, b, c, d) =>

          (if (b < 0 || d >= 53l) 0l else c >> d, a < 0)
      }))
    })

    bindFunction(simpleSignature("bit_sr", 2), fs => {

      FunctionApp.fromDouble(binaryHelperFunction(fs(0).invoke().value(), fs(1).invoke().value(), {
        (a, b, c, d) =>

          (if (b < 0 || d >= 53l) 0l else c << d, a < 0)
      }))
    })

    bindFunction(simpleSignature("bit_rl", 2), fs => {

      FunctionApp.fromDouble(binaryHelperFunction(fs(0).invoke().value(), fs(1).invoke().value(), {
        (a, b, c, d) =>

          (if (b < 0) 0l else (c << (d % 53l)) | (c >> (53l - (d % 53l))), a < 0)
      }))
    })

    bindFunction(simpleSignature("bit_rr", 2), fs => {

      FunctionApp.fromDouble(binaryHelperFunction(fs(0).invoke().value(), fs(1).invoke().value(), {
        (a, b, c, d) =>

          (if (b < 0) 0l else (c >> (d % 53l)) | (c << (53l - (d % 53l))), a < 0)
      }))
    })

    bindFunction(simpleSignature("bit_not", 2), fs => {

      FunctionApp.fromDouble(binaryHelperFunction(fs(0).invoke().value(), 0d, {
        (a, b, c, d) =>

          (~c, a < 0)

      }))
    })

    bindFunction(simpleSignature("bit_and", 2), fs => {

      FunctionApp.fromDouble(binaryHelperFunction(fs(0).invoke().value(), fs(1).invoke().value(), {
        (a, b, c, d) =>

          (c & d, (a < 0 && !(b < 0)) || (!(a < 0) && b < 0))

      }))
    })

    bindFunction(simpleSignature("bit_xor", 2), fs => {

      FunctionApp.fromDouble(binaryHelperFunction(fs(0).invoke().value(), fs(1).invoke().value(), {
        (a, b, c, d) =>

          (c ^ d, (a < 0 && !(b < 0)) || (!(a < 0) && b < 0))

      }))
    })

    bindFunction(simpleSignature("bit_or", 2), fs => {

      FunctionApp.fromDouble(binaryHelperFunction(fs(0).invoke().value(), fs(1).invoke().value(), {
        (a, b, c, d) =>

          (c | d, (a < 0 && !(b < 0)) || (!(a < 0) && b < 0))

      }))
    })

    def binaryHelperFunction(a: Double, b: Double, f: (Double, Double, Long, Long) => (Long, Boolean)): Double = {

      val a_abs = math.abs(a)
      val b_abs = math.abs(b)

      if (a_abs <= 9007199254740991d && b_abs <= 9007199254740991d) {

        val (result, negative) = f(a, b, a_abs.toLong, b_abs.toLong)

        (result & 9007199254740991l).toDouble * (if (negative) -1d else 1d)

      } else
        Double.NaN
    }

    bindFunctions(
      """
        |# {function : symbol priority}
        |
        |{pow       : ^     1900}
        |{pos       : +     1800}
        |{neg       : -     1800}
        |{mul       : *     1700}
        |{mul       : ×     1700}
        |{div       : /     1700}
        |{div       : ÷     1700}
        |{mod       : %     1700}
        |{add       : +     1600}
        |{sub       : -     1600}
        |{bit_sl    : <<    1550}
        |{bit_sl    : ≪    1550}
        |{bit_sr    : >>    1550}
        |{bit_sr    : ≫    1550}
        |{bit_rl    : |<<   1550}
        |{bit_rl    : |≪   1550}
        |{bit_rr    : >>|   1550}
        |{bit_rr    : ≫|   1550}
        |{bit_not   : ~     1540}
        |{bit_and   : &     1530}
        |{bit_xor   : ⊕     1520}
        |{bit_or    : |     1510}
        |{eq        : =     1400}
        |{ne        : <>    1400}
        |{ne        : ≠     1400}
        |{lt        : <     1400}
        |{le        : <=    1400}
        |{le        : ≤     1400}
        |{gt        : >     1400}
        |{ge        : >=    1400}
        |{ge        : ≥     1400}
        |{not       : !     1340}
        |{not       : ¬     1340}
        |{and       : &&    1330}
        |{and       : ∧     1330}
        |{xor       : ⊻     1320}
        |{or        : ||    1310}
        |{or        : ∨     1310}
      """.stripMargin)
  }
}
