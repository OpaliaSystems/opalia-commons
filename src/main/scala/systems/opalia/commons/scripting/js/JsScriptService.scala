package systems.opalia.commons.scripting.js

import org.graalvm.polyglot.{Context, Engine}
import scala.concurrent.duration.Duration
import systems.opalia.interfaces.scripting._


class JsScriptService()
  extends ScriptService {

  final def newScriptEngine(contextTimeout: Duration = Duration.Inf): ScriptEngine = {

    val engine = configEngine(Engine.newBuilder()).build()
    val context = () => createContext(engine)

    new JsScriptEngine(engine, contextTimeout, context)
  }

  protected def configEngine(builder: Engine#Builder): Engine#Builder = {

    builder
  }

  protected def configContext(builder: Context#Builder): Context#Builder = {

    builder
  }

  private def createContext(engine: Engine): Context = {

    val context = configContext(Context.newBuilder()).engine(engine).build()

    context.initialize("js")

    context
  }
}
