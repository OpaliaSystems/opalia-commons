package systems.opalia.commons.scripting.js

import org.graalvm.polyglot.{Context, PolyglotException}
import systems.opalia.interfaces.scripting._
import systems.opalia.interfaces.soa.Terminatable


final class JsScriptSession private[js](underlyingContext: Context)
  extends ScriptSession
    with Terminatable[Unit] {

  protected def shutdownTask(): Unit = {

    try {

      underlyingContext.close()

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def context: ScriptContext = {

    new JsScriptContext(underlyingContext)
  }

  def enter(): Unit = {

    try {

      underlyingContext.enter()

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def leave(): Unit = {

    underlyingContext.leave()
  }
}
