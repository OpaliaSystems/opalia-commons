package systems.opalia.commons.scripting.calculator

import jdk.nashorn.api.scripting.{AbstractJSObject, ScriptObjectMirror}


abstract class FunctionApp private(val signature: FunctionDef.Signature)
  extends AbstractJSObject {

  override def call(thiz: Any, arguments: AnyRef*): AnyRef = {

    val args = arguments.toVector
    val params = signature.parameters.toVector

    if (params.size != args.size)
      throw new CalculatorRuntimeException("Incorrect number of arguments.")

    val apps =
      params.zip(args).map {
        case (param, arg) =>

          arg match {

            case x: ScriptObjectMirror =>
              FunctionApp.fromObjectMirror(param, x)

            case x: FunctionApp =>
              x

            case x =>
              throw new IllegalArgumentException(s"Cannot handle unexpected type ${x.getClass.getName}.")
          }
      }

    invoke(apps)
  }

  override def isFunction: Boolean =
    true

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

  def fromObjectMirror(signature: FunctionDef.Signature, mirror: ScriptObjectMirror): FunctionApp = {

    new FunctionApp(signature) {

      def apply(arguments: Vector[FunctionApp]): Either[FunctionApp, Double] = {

        val target =
          signature.target.getOrElse(FunctionDef.primitiveSignature)

        mirror.call(null, arguments: _*) match {

          case x: ScriptObjectMirror =>
            Left(fromObjectMirror(target, x))

          case x: FunctionApp =>
            Left(x)

          case x: Number =>
            Right(x.doubleValue())

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
