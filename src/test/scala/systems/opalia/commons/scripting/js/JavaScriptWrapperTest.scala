package systems.opalia.commons.scripting.js

import org.graalvm.polyglot.{Context, PolyglotException}
import org.scalatest._
import play.api.libs.json._
import scala.concurrent.duration._
import scala.language.postfixOps
import systems.opalia.commons.json.JsonAstTransformer
import systems.opalia.interfaces.scripting.ScriptException


class JavaScriptWrapperTest
  extends FlatSpec
    with Matchers {

  val scriptService =
    new JsScriptService() {

      override protected def configContext(builder: Context#Builder): Context#Builder =
        builder.allowAllAccess(true)
    }

  val scriptEngine =
    scriptService.newScriptEngine(3 seconds)

  it should "be able to get a JSON document from script" in {

    scriptEngine.withSession {
      session =>

        session.withContext {
          context =>

            context.eval(
              """
                |var obj = {
                | "kay1": "test",
                | "key2": 42,
                | "key3": 42.73,
                | "key4": [1, 2, "three", 4],
                | "key5": null,
                | "key6": {"1": true, "0": false}
                |}
              """.stripMargin)

            JsonAstTransformer.toPlayJson(context.asJson(context.bindings.getMember("obj"))) should be(
              Json.obj(
                "kay1" -> "test",
                "key2" -> 42,
                "key3" -> 42.73,
                "key4" -> Json.arr(1, 2, "three", 4),
                "key5" -> JsNull,
                "key6" -> Json.obj("1" -> true, "0" -> false)
              )
            )
        }
    }
  }

  it should "not be able to get data from other context" in {

    scriptEngine.withSession {
      session =>

        session.withContext {
          context =>

            context.bindings.hasMember("obj") shouldBe false
            an[IllegalArgumentException] should be thrownBy context.bindings.getMember("obj")
        }
    }
  }

  it should "be able to handle context parallel" in {

    val session1 = scriptEngine.newSession()
    val session2 = scriptEngine.newSession()

    try {

      session1.enter()
      session2.enter()

      session1.context.bindings.putMember("a", 42)
      session2.context.bindings.putMember("b", 73)

      session1.context.bindings.getMember("a").asInt should be(42)
      an[IllegalArgumentException] should be thrownBy session1.context.bindings.getMember("b")

      session2.context.bindings.getMember("b").asInt should be(73)
      an[IllegalArgumentException] should be thrownBy session2.context.bindings.getMember("a")

      session1.leave()
      session2.leave()

    } finally {

      session1.shutdown()
      session2.shutdown()
    }
  }

  it should "be able to call Scala functions" in {

    scriptEngine.withSession {
      session =>

        session.withContext {
          context =>

            context.bindings.putMember("foo", () => 42)
            context.bindings.putMember("bar", (a: Int) => 42 * a)
            context.bindings.putMember("baz", (a: Int, b: Int) => 42 * a * b)

            context.eval(
              """
                |var _01 = foo()
                |var _02 = bar(2)
                |var _03 = baz(2, 3)
              """.stripMargin)

            val _01 = context.bindings.getMember("_01").asInt
            val _02 = context.bindings.getMember("_02").asInt
            val _03 = context.bindings.getMember("_03").asInt

            _01 shouldBe a[java.lang.Integer]
            _02 shouldBe a[java.lang.Integer]
            _03 shouldBe a[java.lang.Integer]

            _01 should be(42)
            _02 should be(42 * 2)
            _03 should be(42 * 2 * 3)
        }
    }
  }

  it should "be able to pass through a JSON document" in {

    scriptEngine.withSession {
      session =>

        session.withContext {
          context =>

            val node =
              Json.parse(
                """
                  |{
                  |  "name": "Dresden",
                  |  "location": {
                  |    "latitude": 51.049259,
                  |    "longitude": 13.73836,
                  |    "altitude": null
                  |  },
                  |  "info": [
                  |    {
                  |      "name": "Population",
                  |      "date": "2015-12-31",
                  |      "value": 543825
                  |    },
                  |    {
                  |      "name" : "Area",
                  |      "city": 328.48,
                  |      "capital of Germany": false,
                  |      "capital of Saxony": true
                  |    }
                  |  ]
                  |}
                """.stripMargin)

            context.bindings.putMember("obj", context.asValue(JsonAstTransformer.fromPlayJson(node)))

            JsonAstTransformer.toPlayJson(context.asJson(context.bindings.getMember("obj"))) shouldBe node
        }
    }
  }

  it should "be able to stop less trusted script code" in {

    try {

      scriptEngine.withSession {
        session =>

          session.withContext {
            context =>

              context.eval("while(true);")
          }
      }

    } catch {

      case e: ScriptException =>
        e.getCause.asInstanceOf[PolyglotException].isCancelled shouldBe true
    }
  }
}
