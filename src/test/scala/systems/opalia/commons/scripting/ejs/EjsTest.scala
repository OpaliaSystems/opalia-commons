package systems.opalia.commons.scripting.ejs

import java.io.IOException
import java.nio.file.{Path, Paths}
import org.scalatest.flatspec._
import org.scalatest.matchers.should._
import scala.collection.immutable.ListMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source
import systems.opalia.commons.scripting.js.JsScriptService
import systems.opalia.interfaces.json.JsonAst
import systems.opalia.interfaces.scripting._


class EjsTest
  extends AnyFlatSpec
    with Matchers {

  val scriptEngine: ScriptEngine =
    (new JsScriptService()).newScriptEngine()

  val files = Map(

    Paths.get("/utils/escape.ejs") ->
      """
        |<%
        |utils.escape = function(html) {
        |    return String(html)
        |        .replace(/&(?!#?[a-zA-Z0-9]+;)/g, '&amp;')
        |        .replace(/</g, '&lt;')
        |        .replace(/>/g, '&gt;')
        |        .replace(/"/g, '&#34;')
        |        .replace(/'/g, '&#39;')
        |};
        |-%>
      """.stripMargin.trim,

    Paths.get("/html/inc/main.html.ejs") ->
      """
        |<% include ../../utils/escape.ejs -%>
        |<% include header.html.ejs -%>
        |<% include footer.html.ejs -%>
        |<% macros.main = function(title, contentBody) { -%>
        |<!DOCTYPE html>
        |<html>
        |<head>
        |    <meta charset="UTF-8">
        |    <title><%= title %></title>
        |</head>
        |<body>
        |<% macros.header(); -%>
        |    <div id="content">
        |<% contentBody(); -%>
        |    </div>
        |<% macros.footer(); -%>
        |</body>
        |<!-- <%- JSON.stringify(locals) %> -->
        |</html>
        |<% } -%>
      """.stripMargin.trim,

    Paths.get("/html/inc/header.html.ejs") ->
      """
        |<% macros.header = function() { -%>
        |    <header></header>
        |<% } -%>
      """.stripMargin.trim,

    Paths.get("/html/inc/footer.html.ejs") ->
      """
        |<% macros.footer = function() { -%>
        |    <footer></footer>
        |<% } -%>
      """.stripMargin.trim,

    Paths.get("/html/content/success.html.ejs") ->
      """
        |<% include ../inc/main.html.ejs -%>
        |<% macros.main(locals.title, content); -%>
        |<% function content() { -%>
        |<%# this is a comment -%>
        |        <h1><%= locals.title %></h1>
        |        <p><%%= <%= locals.text %> %></p>
        |<% } -%>
      """.stripMargin.trim,

    Paths.get("/html/content/failure1.html.ejs") ->
      """
        |<% include ../inc/main.html.ejs -%>
        |<% macros.main(locals.title, content); -%>
        |<% function content() { -%>
        |<%# this is a comment -%>
        |<%- }} -%>
        |<% } -%>
      """.stripMargin.trim,

    Paths.get("/html/content/failure2.html.ejs") ->
      """
        |<% include ../inc/not_existing_file.html.ejs -%>
        |<% macros.main(locals.title, content); -%>
        |<% function content() { -%>
        |<% } -%>
      """.stripMargin.trim,

    Paths.get("/html/content/failure3.html.ejs") ->
      """
        |<% include ../inc/main.html.ejs -%>
        |<% macros.main(locals.title, content); -%>
        |<% function content() { -%>
        |<%# this is a comment -%>
        |<%
        |
        |
        |   locals.blubb.bla = 42;
        |
        |-%>
        |<% } -%>
      """.stripMargin.trim)

  val data =
    JsonAst.JsonObject(ListMap(
      "title" -> JsonAst.JsonString("Page Title"),
      "text" -> JsonAst.JsonString("Hello from EJS!")
    ))

  val ejs =
    Ejs(scriptEngine, new EjsConfiguration {

      val openWith = EjsDefaultConfiguration.openWith
      val closeWith = EjsDefaultConfiguration.closeWith

      def resolve(path: Path)
                 (implicit executor: ExecutionContext): Future[Source] =
        Future.successful(files.get(path).map(Source.fromString)
          .getOrElse(throw new IOException(s"Cannot resolve path $path.")))
    })

  it should "generate the same result for compiling and direct rendering" in {

    Await.result(ejs.render(Paths.get("/html/content/success.html.ejs"), data), Duration.Inf) should be(
      Await.result(ejs.compile(Paths.get("/html/content/success.html.ejs")).flatMap(_.render(data)), Duration.Inf))
  }

  it should "generate a valid document" in {

    Await.result(ejs.render(Paths.get("/html/content/success.html.ejs"), data), Duration.Inf) should be(
      """
        |<!DOCTYPE html>
        |<html>
        |<head>
        |    <meta charset="UTF-8">
        |    <title>Page Title</title>
        |</head>
        |<body>
        |    <header></header>
        |    <div id="content">
        |        <h1>Page Title</h1>
        |        <p><%= Hello from EJS! %></p>
        |    </div>
        |    <footer></footer>
        |</body>
        |<!-- {"title":"Page Title","text":"Hello from EJS!"} -->
        |</html>
      """.stripMargin.trim + "\n")
  }

  it should "throw an exception for errors occurred while execution" in {

    val thrownRendering = intercept[ScriptException] {

      Await.result(ejs.render(Paths.get("/html/content/failure3.html.ejs"), data), Duration.Inf)
    }

    val thrownCompiling = intercept[ScriptException] {

      Await.result(ejs.compile(Paths.get("/html/content/failure3.html.ejs")).flatMap(_.render(data)), Duration.Inf)
    }

    val message = "TypeError: in /html/content/failure3.html.ejs" +
      " on line 8: Cannot set property 'bla' of undefined"

    thrownRendering.getMessage should be(message)

    thrownCompiling.getMessage should be(message)
  }

  it should "throw an exception while rendering a syntax error" in {

    val thrownRendering = intercept[ScriptException] {

      Await.result(ejs.render(Paths.get("/html/content/failure1.html.ejs"), JsonAst.JsonNull), Duration.Inf)
    }

    val thrownCompiling = intercept[ScriptException] {

      Await.result(ejs.compile(Paths.get("/html/content/failure1.html.ejs")).flatMap(_.render(data)), Duration.Inf)
    }

    val message = "SyntaxError: Expected an operand but found }"

    thrownRendering.getMessage should be(message)

    thrownCompiling.getMessage should be(message)
  }

  it should "throw an io exception while resolving a not existing file" in {

    val thrownRendering = intercept[IOException] {

      Await.result(ejs.render(Paths.get("/html/content/failure2.html.ejs"), JsonAst.JsonNull), Duration.Inf)
    }

    val thrownCompiling = intercept[IOException] {

      Await.result(ejs.compile(Paths.get("/html/content/failure2.html.ejs")), Duration.Inf)
    }

    val message = "Cannot resolve path /html/inc/not_existing_file.html.ejs."

    thrownRendering.getMessage should be(message)

    thrownCompiling.getMessage should be(message)
  }
}
