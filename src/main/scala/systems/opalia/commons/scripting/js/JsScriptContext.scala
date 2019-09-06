package systems.opalia.commons.scripting.js

import org.graalvm.polyglot.{Context, PolyglotException, Source, Value}
import scala.collection.immutable.ListMap
import systems.opalia.interfaces.json.JsonAst
import systems.opalia.interfaces.scripting._


final class JsScriptContext private[js](underlyingContext: Context)
  extends ScriptContext
    with Conversion {

  /* this implementation based on GraalVM SDK
   * https://github.com/oracle/graal
   * https://www.graalvm.org/docs/reference-manual/embed/
   * https://www.graalvm.org/sdk/javadoc/index.html
   */

  def bindings: ScriptValue = {

    try {

      new JsScriptValue(underlyingContext.getBindings("js"))

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def eval(script: String, name: String): ScriptValue = {

    try {

      val source = Source.newBuilder("js", script, name).buildLiteral()

      new JsScriptValue(underlyingContext.eval(source))

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def eval(script: String): ScriptValue = {

    try {

      val source = Source.create("js", script)

      new JsScriptValue(underlyingContext.eval(source))

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def eval(script: ScriptCompilation): ScriptValue = {

    val compilation =
      script match {
        case x: JsScriptCompilation => x
        case _ => throw new IllegalArgumentException("Cannot handle script compilation from another implementation.")
      }

    try {

      new JsScriptValue(underlyingContext.eval(compilation.source))

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def asValue(value: Any): ScriptValue = {

    value match {
      case x: ScriptValue => x
      case x => new JsScriptValue(underlyingContext.asValue(prepareForPolyglot(x)))
    }
  }

  def asValue(node: JsonAst.JsonValue): ScriptValue = {

    def fromJson(node: JsonAst.JsonValue): Value = {

      node match {

        case JsonAst.JsonNull =>
          underlyingContext.asValue(null)

        case JsonAst.JsonBoolean(x) =>
          underlyingContext.asValue(x)

        case JsonAst.JsonString(x) =>
          underlyingContext.asValue(x)

        case JsonAst.JsonNumberByte(x) =>
          underlyingContext.asValue(x)

        case JsonAst.JsonNumberShort(x) =>
          underlyingContext.asValue(x)

        case JsonAst.JsonNumberInt(x) =>
          underlyingContext.asValue(x)

        case JsonAst.JsonNumberLong(x) =>
          underlyingContext.asValue(x)

        case JsonAst.JsonNumberFloat(x) =>
          underlyingContext.asValue(x)

        case JsonAst.JsonNumberDouble(x) =>
          underlyingContext.asValue(x)

        case JsonAst.JsonNumberBigInt(x) =>
          underlyingContext.asValue(x.toDouble)

        case JsonAst.JsonNumberBigDecimal(x) =>
          underlyingContext.asValue(x.toDouble)

        case JsonAst.JsonArray(x) => {

          val bindings = underlyingContext.eval("js", "new Array()")

          for ((v, k) <- x.zipWithIndex)
            bindings.setArrayElement(k, fromJson(v))

          bindings
        }

        case JsonAst.JsonObject(x) => {

          val bindings = underlyingContext.eval("js", "new Object()")

          for ((k, v) <- x)
            bindings.putMember(k, fromJson(v))

          bindings
        }
      }
    }

    try {

      new JsScriptValue(fromJson(node))

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def asJson(value: Any): JsonAst.JsonValue = {

    asJson(asValue(value))
  }

  def asJson(value: ScriptValue): JsonAst.JsonValue = {

    value match {

      case x if (x.isNull) =>
        JsonAst.JsonNull

      case x if (x.isBoolean) =>
        JsonAst.JsonBoolean(x.asBoolean)

      case x if (x.isNumber && x.fitsInLong) =>
        JsonAst.JsonNumberLong(x.asLong)

      case x if (x.isNumber && x.fitsInDouble) =>
        JsonAst.JsonNumberDouble(x.asDouble)

      case x if (x.isNumber) =>
        JsonAst.JsonNumberBigDecimal(x.as(classOf[BigDecimal]))

      case x if (x.isString) =>
        JsonAst.JsonString(x.asString)

      case x if (x.hasArrayElements) =>
        JsonAst.JsonArray(
          for (i <- 0 until x.getArraySize) yield asJson(x.getArrayElement(i))
        )

      case x if (x.hasMembers) =>
        JsonAst.JsonObject(ListMap((
          for (key <- x.getMemberKeys.toIndexedSeq) yield (key -> asJson(x.getMember(key)))
          ): _*))

      case x =>
        throw new IllegalArgumentException(s"Cannot convert from $x (${x.getClass.getName}) to JSON.")
    }
  }
}
