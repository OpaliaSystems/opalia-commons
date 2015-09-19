package systems.opalia.commons.scripting

import javax.script._
import jdk.nashorn.api.scripting._
import play.api.libs.json._
import scala.collection.JavaConverters._


class JavaScript private(withJava: Boolean) {

  private val engine =
    if (withJava)
      (new NashornScriptEngineFactory()).getScriptEngine("-strict")
    else
      (new NashornScriptEngineFactory()).getScriptEngine("-strict", "--no-java", "--no-syntax-extensions")

  def compile(script: String): CompiledScript =
    engine.asInstanceOf[Compilable].compile(script)

  def newContext(): JavaScript.Context =
    new JavaScriptContext()

  class JavaScriptContext
    extends JavaScript.Context {

    private val context = new SimpleScriptContext()
    private val bindings = engine.createBindings

    context.setBindings(bindings, ScriptContext.ENGINE_SCOPE)

    def eval(script: CompiledScript): Any =
      script.eval(getBindings(context))

    def eval(script: String): Any =
      engine.eval(script, getBindings(context))

    def evalJson(script: CompiledScript): JsValue =
      toJson(script.eval(getBindings(context)))

    def evalJson(script: String): JsValue =
      toJson(engine.eval(script, getBindings(context)))

    def contains(key: String): Boolean =
      getBindings(context).containsKey(key)

    def get(key: String): Option[Any] =
      Option(getBindings(context).get(key))

    def getJson(key: String): Option[JsValue] =
      Option(getBindings(context).get(key)).map(toJson)

    def put(key: String, value: Any): Unit =
      getBindings(context).put(key, value)

    def putJson(key: String, value: JsValue): Unit =
      getBindings(context).put(key, fromJson(value))

    def putAll(entry: (String, Any)*): Unit =
      getBindings(context).putAll(entry.map(x => x._1 -> x._2).toMap.asJava)

    def putAllJson(entry: (String, JsValue)*): Unit =
      getBindings(context).putAll(entry.map(x => x._1 -> fromJson(x._2)).toMap.asJava)

    def remove(key: String): Unit =
      getBindings(context).remove(key)

    private def getBindings(context: ScriptContext): Bindings =
      context.getBindings(ScriptContext.ENGINE_SCOPE)

    private def toJson(value: Any): JsValue =
      value match {

        case x: Boolean => JsBoolean(x)

        case x: Byte => JsNumber(BigDecimal.decimal(x))

        case x: Short => JsNumber(BigDecimal.decimal(x))

        case x: Int => JsNumber(BigDecimal.decimal(x))

        case x: Long => JsNumber(BigDecimal.decimal(x))

        case x: Float => JsNumber(BigDecimal.decimal(x))

        case x: Double => JsNumber(BigDecimal.decimal(x))

        case x: BigInt => JsNumber(BigDecimal(x))

        case x: BigDecimal => JsNumber(x)

        case x: Char => JsString(x.toString)

        case x: String => JsString(x)

        case x: ScriptObjectMirror => {

          if (x.isFunction)
            JsNull
          else if (x.isArray)
            JsArray(x.values.asScala.toSeq.map(toJson))
          else
            JsObject(x.asScala.toSeq.map(y => (y._1, toJson(y._2))))
        }
        case _ =>
          JsNull
      }

    private def fromJson(value: JsValue): Any =
      value match {

        case JsString(x) => x

        case JsBoolean(x) => x

        case JsNumber(x) => x.toDouble

        case JsArray(x) => {

          val bindings = eval("new Array()").asInstanceOf[Bindings]

          for ((v, k) <- x.zipWithIndex)
            bindings.put(k.toString, fromJson(v))

          bindings
        }

        case JsObject(x) => {

          val bindings = eval("new Object()").asInstanceOf[Bindings]

          for ((k, v) <- x)
            bindings.put(k, fromJson(v))

          bindings
        }

        case JsNull =>
          null
      }
  }

}

object JavaScript {

  def apply(withJava: Boolean = false): JavaScript =
    new JavaScript(withJava)

  trait Context {

    def eval(script: CompiledScript): Any

    def eval(script: String): Any

    def evalJson(script: CompiledScript): JsValue

    def evalJson(script: String): JsValue

    def contains(key: String): Boolean

    def get(key: String): Option[Any]

    def getJson(key: String): Option[JsValue]

    def put(key: String, value: Any): Unit

    def putJson(key: String, value: JsValue): Unit

    def putAll(entry: (String, Any)*): Unit

    def putAllJson(entry: (String, JsValue)*): Unit

    def remove(key: String): Unit
  }

}
