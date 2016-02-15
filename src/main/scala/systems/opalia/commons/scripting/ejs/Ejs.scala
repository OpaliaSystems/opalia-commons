package systems.opalia.commons.scripting.ejs

import java.nio.file.Path
import javax.script._
import play.api.libs.json._
import scala.concurrent.{ExecutionContext, Future}
import systems.opalia.commons.scripting.JavaScript
import systems.opalia.commons.scripting.ejs.exceptions._


class Ejs private(js: JavaScript,
                  config: EjsConfiguration)
                 (implicit executor: ExecutionContext) {

  def compile(path: Path): Future[Template] = {

    val parser = new Parser(config)

    parser.parse(path).map {
      parsed =>

        try {

          new Template(js.compile(parsed))

        } catch {

          case e: ScriptException =>
            throw new EjsParsingException(stripMessage(e.getMessage))
        }
    }
  }

  def render(path: Path, args: JsValue): Future[String] = {

    val parser = new Parser(config)
    val context = js.newContext()

    parser.parse(path).map {
      parsed =>

        withErrorHandling(context) {

          context.putJson("locals", args)
          context.evalJson(parsed)
        }
    }
  }

  class Template(script: CompiledScript) {

    def render(args: JsValue): Future[String] =
      Future {

        val context = js.newContext()

        withErrorHandling(context) {

          context.putJson("locals", args)
          context.evalJson(script)
        }
      }
  }

  private def withErrorHandling(context: JavaScript.Context)(block: => JsValue): String = {

    try {

      block match {

        case JsString(x) => x
        case _ => throw new IllegalArgumentException("The EJS script must return a string.")
      }

    } catch {

      case e: ScriptException => {

        val (stackInfo, errorInfo) =
          (context.getJson("__stack").getOrElse(JsNull), context.getJson("__error").getOrElse(JsNull))

        val (documentPath, documentLineno, relativeLineno, name, message, lineno) =
          (for {
            documentPath <- (stackInfo \ "document_path").asOpt[String]
            documentLineno <- (stackInfo \ "document_lineno").asOpt[Int]
            relativeLineno <- (stackInfo \ "relative_lineno").asOpt[Int]
            name <- (errorInfo \ "name").asOpt[String]
            message <- (errorInfo \ "message").asOpt[String]
            lineno <- (errorInfo \ "lineno").asOpt[Int]
          } yield (documentPath, documentLineno, relativeLineno, name, message, lineno))
            .getOrElse(throw new EjsParsingException(stripMessage(e.getMessage)))

        throw new EjsRunningException("EJS " + name + " in " + documentPath + " on line "
          + (documentLineno + lineno - relativeLineno) + ": " + message)
      }
    }
  }

  private def stripMessage(message: String): String = {

    val pattern = """^\s*<eval>:\d+:\d+\s+(.*)""".r

    message.lines.toSeq.head match {

      case pattern(x) =>
        "EJS Error: " + x

      case _ =>
        throw new IllegalArgumentException(message)
    }
  }
}

object Ejs {

  def apply(js: JavaScript, config: EjsConfiguration = EjsDefaultConfiguration)
           (implicit executor: ExecutionContext): Ejs =
    new Ejs(js, config)
}
