package systems.opalia.commons.scripting.ejs

import java.nio.file.Path
import org.apache.commons.lang3.StringEscapeUtils
import scala.concurrent.{ExecutionContext, Future}


protected final class Parser(config: EjsConfiguration)
                            (implicit executor: ExecutionContext) {

  // https://github.com/mde/ejs

  private object Status
    extends Enumeration {

    val Raw,
    EmbeddedScriptlet,
    EmbeddedEscaped,
    EmbeddedUnescaped,
    EmbeddedCommented = Value
  }

  private object Snippets {

    def beginFunction(): Seq[String] =
      """
        |var __stack = { 'document_path': undefined, 'document_lineno': undefined, 'relative_lineno': undefined };
        |var __error = { 'name': undefined, 'message': undefined, 'lineno': undefined };
        |var __buffer = [];
        |
        |(function(locals) {
        |
        |    var utils = {};
        |    var macros = {};
        |    var cache = {};
        |
        |    try {
        |
      """.stripMargin.trim.lines.toSeq

    def endFunction(): Seq[String] =
      """
        |
        |        return __buffer.join('');
        |
        |    } catch (error) {
        |
        |        __error.name = error.name;
        |        __error.message = error.message;
        |        __error.lineno = error.lineNumber;
        |
        |        throw error;
        |    }
        |
        |})(locals || {});
      """.stripMargin.trim.lines.toSeq

    def beginScope(): Seq[String] =
      Seq("""(function() {""")

    def endScope(): Seq[String] =
      Seq("""})();""")

    def scriptlet(data: String): Seq[String] =
      data.lines.toSeq

    def emptyLine(): Seq[String] =
      Seq("")

    def bufferNewLine(): Seq[String] =
      Seq( """__buffer.push('\n');""")

    def bufferText(data: String, escape: Boolean): Seq[String] =
      if (escape)
        Seq("""__buffer.push( utils.escape( (""" + data + """) ) );""")
      else
        Seq("""__buffer.push( (""" + data + """) );""")

    def bufferTextRaw(data: String, escape: Boolean): Seq[String] =
      if (escape)
        Seq("""__buffer.push( utils.escape( ('""" + StringEscapeUtils.escapeEcmaScript(data) + """') ) );""")
      else
        Seq("""__buffer.push( ('""" + StringEscapeUtils.escapeEcmaScript(data) + """') );""")

    def documentPath(document: String): Seq[String] =
      Seq("""__stack.document_path = '""" + StringEscapeUtils.escapeEcmaScript(document) + """';""")

    def documentLineNumber(lineno: Int): Seq[String] =
      Seq("""__stack.document_lineno = """ + lineno + """;""")

    def relativeLineNumber(lineno: Int): Seq[String] =
      Seq("""__stack.relative_lineno = """ + lineno + """;""")
  }

  def parse(path: Path): Future[String] = {

    val prefix = Snippets.beginFunction()
    val postfix = Snippets.endFunction()

    parseDocument(prefix, path, None)
      .map(_ ++ postfix)
      .map(_.mkString("\n"))
  }

  private def parseDocument(process: Seq[String],
                            pathNew: Path,
                            pathOld: Option[Path]): Future[Seq[String]] = {

    config.resolve(pathNew)
      .map(_.getLines.toSeq)
      .flatMap {
        lines =>

          val prefix = process ++ Snippets.documentPath(pathNew.toString) ++ Snippets.beginScope()
          val postfix = Snippets.endScope() ++ pathOld.map((x) => Snippets.documentPath(x.toString)).getOrElse(Nil)

          parseLines(prefix, lines, pathNew)
            .map(_ ++ postfix)
      }
  }

  private def parseLines(process: Seq[String],
                         lines: Seq[String],
                         path: Path): Future[Seq[String]] = {

    def parse(process: Seq[String],
              lines: Seq[String],
              status: Status.Value,
              lineno: Int): Future[Seq[String]] =
      lines.toList match {

        case head :: tail => {

          val prefix =
            process ++
              (if (status != Status.EmbeddedScriptlet)
                Snippets.documentLineNumber(lineno) ++ Snippets.relativeLineNumber(process.length + 3)
              else
                Nil)

          parseLine(prefix, head, status, lineno, path).flatMap {
            case (infix, newStatus) =>

              parse(infix, tail, newStatus, lineno + 1)
          }
        }

        case Nil =>
          Future.successful(process)
      }

    parse(process, lines, Status.Raw, 1)
  }

  private def parseLine(process: Seq[String],
                        line: String,
                        status: Status.Value,
                        lineno: Int,
                        path: Path): Future[(Seq[String], Status.Value)] = {

    val patternEmbeddedEscaped = """^(=)(.*)""".r
    val patternEmbeddedUnescaped = """^(-)(.*)""".r
    val patternEmbeddedCommented = """^(#)(.*)""".r
    val patternLiteral = """^(%)(.*)""".r
    val patternInclusion = """^(\s*include\s+)(.*)""".r

    def parse(process: Seq[String],
              line: String,
              status: Status.Value): Future[(Seq[String], Status.Value)] = {

      if (status == Status.Raw) {

        val openAt = line.indexOf(config.openWith)

        if (line.isEmpty)
          return Future.successful(process ++ Snippets.bufferNewLine(), status)

        if (openAt == -1)
          return Future.successful(process ++ Snippets.bufferTextRaw(line, false) ++ Snippets.bufferNewLine(), status)

        val firstPart = line.take(openAt)
        val secondPart = line.drop(openAt + config.openWith.length)

        val (statusNew, lineSub) =
          secondPart match {
            case patternEmbeddedEscaped(_, suffix) =>
              (Status.EmbeddedEscaped, suffix)
            case patternEmbeddedUnescaped(_, suffix) =>
              (Status.EmbeddedUnescaped, suffix)
            case patternLiteral(_, suffix) =>
              (Status.Raw, suffix)
            case patternEmbeddedCommented(_, suffix) =>
              (Status.EmbeddedCommented, suffix)
            case _ =>
              (Status.EmbeddedScriptlet, secondPart)
          }

        val processNew =
          process ++
            (if (!firstPart.isEmpty)
              Snippets.bufferTextRaw(firstPart, false)
            else
              Nil) ++
            (if (statusNew == Status.Raw)
              Snippets.bufferTextRaw(config.openWith, false)
            else
              Nil)

        parse(processNew, lineSub, statusNew)

      } else {

        def handleEmbedded(process: Seq[String], text: String): Future[Seq[String]] =
          text match {

            case patternInclusion(_, suffix) if (status == Status.EmbeddedScriptlet) =>
              parseDocument(process, path.resolveSibling(suffix.trim).normalize, Some(path))

            case _ => {

              if (status == Status.EmbeddedScriptlet)
                Future.successful(process ++ Snippets.scriptlet(text))
              else if (status == Status.EmbeddedEscaped)
                Future.successful(process ++ Snippets.bufferText(text, true))
              else if (status == Status.EmbeddedUnescaped)
                Future.successful(process ++ Snippets.bufferText(text, false))
              else
                Future.successful(process)
            }
          }

        val closeAt = line.indexOf(config.closeWith)

        if (line.isEmpty)
          return Future.successful(process ++ Snippets.emptyLine(), status)

        if (closeAt == -1)
          return handleEmbedded(process, line)
            .map((_, status))

        val firstPart = line.take(closeAt)
        val secondPart = line.drop(closeAt + config.closeWith.length)

        if (firstPart.takeRight(1) == "-") {

          handleEmbedded(process, firstPart.dropRight(1))
            .flatMap {
              process =>

                if (secondPart.isEmpty)
                  Future.successful(process, Status.Raw)
                else
                  parse(process, secondPart, Status.Raw)
            }

        } else {

          handleEmbedded(process, firstPart)
            .flatMap {
              process =>

                parse(process, secondPart, Status.Raw)
            }
        }
      }
    }

    parse(process, line, status)
  }
}
