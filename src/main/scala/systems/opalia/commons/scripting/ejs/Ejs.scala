package systems.opalia.commons.scripting.ejs

import java.nio.file.Path
import scala.concurrent.{ExecutionContext, Future}
import systems.opalia.commons.json.JsonAstTransformer
import systems.opalia.interfaces.json.JsonAst
import systems.opalia.interfaces.scripting._


class Ejs private(scriptEngine: ScriptEngine,
                  config: EjsConfiguration)
                 (implicit executor: ExecutionContext) {

  def compile(path: Path): Future[Template] = {

    val parser = new Parser(config)

    parser.parse(path).map {
      parsed =>

        new Template(scriptEngine.compile(parsed))
    }
  }

  def render(path: Path, args: JsonAst.JsonValue): Future[String] = {

    val parser = new Parser(config)

    parser.parse(path).map {
      parsed =>

        scriptEngine.withSession {
          session =>

            session.withContext {
              context =>

                withErrorHandling(context) {

                  context.bindings.putMember("locals", context.asValue(args))
                  context.eval(parsed).asString
                }
            }
        }
    }
  }

  class Template(script: ScriptCompilation) {

    def render(args: JsonAst.JsonValue): Future[String] =
      Future {

        scriptEngine.withSession {
          session =>

            session.withContext {
              context =>

                withErrorHandling(context) {

                  context.bindings.putMember("locals", context.asValue(args))
                  context.eval(script).asString
                }
            }
        }
      }
  }

  private def withErrorHandling(context: ScriptContext)(block: => String): String = {

    try {

      block

    } catch {

      case _: ScriptException if (context.bindings.hasMember("__stack") && context.bindings.hasMember("__error")) => {

        val stackInfo = JsonAstTransformer.toPlayJson(context.asJson(context.bindings.getMember("__stack")))
        val errorInfo = JsonAstTransformer.toPlayJson(context.asJson(context.bindings.getMember("__error")))

        val documentPath = (stackInfo \ "document_path").as[String]
        val documentLineno = (stackInfo \ "document_lineno").as[Int]
        val relativeLineno = (stackInfo \ "relative_lineno").as[Int]
        val name = (errorInfo \ "name").as[String]
        val message = (errorInfo \ "message").as[String]
        val stack = (errorInfo \ "stack").as[String]

        throw new ScriptException(name + ": in " + documentPath + " on line " +
          (documentLineno + getPositionFromStack(stack)._1 - relativeLineno) + ": " + message)
      }

      case e: ScriptException =>

        throw new ScriptException(e.getMessage.lines.toSeq.head.replaceFirst(""" Unnamed:\d+:\d+""", ""), e)
    }
  }

  private def getPositionFromStack(stack: String): (Int, Int) = {

    val pattern = """.*:([1-9][0-9]*):([1-9][0-9]*)\)?$""".r

    stack.lines.toSeq.apply(1) match {
      case pattern(line, column) => (line.toInt, column.toInt)
      case _ => throw new IllegalArgumentException(s"Cannot parse stack: $stack")
    }
  }
}

object Ejs {

  def apply(scriptEngine: ScriptEngine, config: EjsConfiguration = EjsDefaultConfiguration)
           (implicit executor: ExecutionContext): Ejs =
    new Ejs(scriptEngine, config)
}
