package systems.opalia.commons.scripting.js

import java.util.{Timer, TimerTask}
import org.graalvm.polyglot.{Context, Engine, Source}
import scala.concurrent.duration.Duration
import systems.opalia.interfaces.scripting._


final class JsScriptEngine private[js](engine: Engine, contextTimeout: Duration, createContext: () => Context)
  extends ScriptEngine {

  val timer = new Timer(true)

  def newSession(): ScriptSession = {

    val context = createContext()
    val session = new JsScriptSession(context)

    if (contextTimeout.isFinite()) {

      timer.schedule(new TimerTask() {

        def run(): Unit = {

          context.close(true)
          session.shutdown()
        }

      }, contextTimeout.toMillis)
    }

    session
  }

  def compile(script: String, name: String): ScriptCompilation = {

    val source = Source.newBuilder("js", script, name).buildLiteral()

    new JsScriptCompilation(source)
  }

  def compile(script: String): ScriptCompilation = {

    val source = Source.create("js", script)

    new JsScriptCompilation(source)
  }
}
