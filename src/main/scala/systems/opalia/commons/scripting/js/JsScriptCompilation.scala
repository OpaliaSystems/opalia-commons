package systems.opalia.commons.scripting.js

import org.graalvm.polyglot.Source
import systems.opalia.interfaces.scripting._


final class JsScriptCompilation private[js](private[js] val source: Source)
  extends ScriptCompilation {
}
