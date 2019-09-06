package systems.opalia.commons.scripting.calculator

import systems.opalia.interfaces.scripting._


abstract class FunctionApp private(val signature: FunctionDef.Signature)
  extends ExecutableProxy {

  def apply(arguments: Seq[ScriptValue]): AnyRef = {

    val args = arguments.toVector
    val params = signature.parameters.toVector

    if (params.size != args.size)
      throw new CalculatorRuntimeException("Incorrect number of arguments.")

    val apps =
      params.zip(args).map {
        case (param, arg) =>

          arg match {

            case x if (x.canExecute) =>
              FunctionApp.fromScriptValue(param, x)

            case x if (x.isForeignObject && x.asForeignObject.isInstanceOf[FunctionApp]) =>
              x.asForeignObject.asInstanceOf[FunctionApp]

            case x =>
              throw new IllegalArgumentException(s"Cannot handle unexpected type ${x.getClass.getName}.")
          }
      }

    invoke(apps)
  }

  def invoke(arguments: Vector[FunctionApp] = Vector.empty): FunctionApp = {

    if (signature.parameters.size != arguments.size)
      throw new CalculatorRuntimeException(s"Incorrect number of arguments.")

    if (!signature.parameters.zip(arguments).forall(x => x._1.verify(x._2.signature)))
      throw new CalculatorRuntimeException(s"Cannot verify signature.")

    apply(arguments) match {

      case Left(x) => {

        if (!signature.target.getOrElse(FunctionDef.primitiveSignature).verify(x.signature))
          throw new CalculatorRuntimeException("Cannot verify target part of signature.")

        x
      }

      case Right(_) =>
        throw new CalculatorRuntimeException("Expect function to continue.")
    }
  }

  def value(): Double = {

    if (!signature.verify(FunctionDef.primitiveSignature))
      throw new CalculatorRuntimeException(s"Cannot verify signature.")

    apply() match {

      case Left(_) =>
        throw new CalculatorRuntimeException("Expect value to continue.")

      case Right(x) =>
        x
    }
  }

  protected def apply(arguments: Vector[FunctionApp] = Vector.empty): Either[FunctionApp, Double]
}

object FunctionApp {

  def fromScriptValue(signature: FunctionDef.Signature, scriptValue: ScriptValue): FunctionApp = {

    new FunctionApp(signature) {

      def apply(arguments: Vector[FunctionApp]): Either[FunctionApp, Double] = {

        val target =
          signature.target.getOrElse(FunctionDef.primitiveSignature)

        scriptValue.execute(arguments: _*) match {

          case x if (x.canExecute) =>
            Left(fromScriptValue(target, x))

          case x if (x.isForeignObject && x.asForeignObject.isInstanceOf[FunctionApp]) =>
            Left(x.asForeignObject.asInstanceOf[FunctionApp])

          case x if (x.isNumber) =>
            Right(x.asDouble)

          case x =>
            throw new IllegalArgumentException(s"Cannot handle unexpected type ${x.getClass.getName}.")
        }
      }
    }
  }

  def fromFunction(signature: FunctionDef.Signature, f: (Vector[FunctionApp]) => FunctionApp): FunctionApp = {

    new FunctionApp(signature) {

      def apply(arguments: Vector[FunctionApp]): Either[FunctionApp, Double] = {

        Left(f(arguments))
      }
    }
  }

  def fromDouble(double: Double): FunctionApp = {

    new FunctionApp(FunctionDef.primitiveSignature) {

      def apply(arguments: Vector[FunctionApp]): Either[FunctionApp, Double] = {

        Right(double)
      }
    }
  }

  def wrap(app: FunctionApp): FunctionApp = {

    val signature =
      if (app.signature.verify(FunctionDef.primitiveSignature))
        None
      else
        Some(app.signature)

    new FunctionApp(FunctionDef.Signature("", Nil, signature)) {

      def apply(arguments: Vector[FunctionApp]): Either[FunctionApp, Double] = {

        Left(app)
      }
    }
  }
}
