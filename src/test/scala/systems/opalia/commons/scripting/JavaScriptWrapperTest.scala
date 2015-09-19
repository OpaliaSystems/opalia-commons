package systems.opalia.commons.scripting

import org.scalatest._
import play.api.libs.json._


class JavaScriptWrapperTest
  extends FlatSpec
    with Matchers {

  val js = JavaScript()

  it should "be able to get a JSON document from script" in {

    val context = js.newContext()

    context.eval(
      """
        |var obj = {
        | "kay1": "test",
        | "key2": 42,
        | "key3": 42.73,
        | "key4": [1, 2, "three", 4]
        |}
      """.stripMargin)

    context.getJson("obj").get should be(
      Json.obj(
        "kay1" -> "test",
        "key2" -> 42,
        "key3" -> 42.73,
        "key4" -> Json.arr(1, 2, "three", 4))
    )
  }

  it should "not be able to get data from other context" in {

    val context = js.newContext()

    context.getJson("obj") shouldBe None
  }

  it should "be able to handle context parallel" in {

    val context1 = js.newContext()
    val context2 = js.newContext()

    context1.put("a", 42)
    context2.put("b", 73)

    context1.get("a") should be(Some(42))
    context2.get("a") should be(None)
    context1.get("b") should be(None)
    context2.get("b") should be(Some(73))
  }

  it should "be able to call Scala functions" in {

    val context = js.newContext()

    context.put("foo", () => 42)
    context.put("bar", (a: Int) => 42 * a)
    context.put("baz", (a: Int, b: Int) => 42 * a * b)

    context.eval(
      """
        |var _01 = foo()
        |var _02 = bar(2)
        |var _03 = baz(2, 3)
      """.stripMargin)

    val _01 = context.get("_01").get
    val _02 = context.get("_02").get
    val _03 = context.get("_03").get

    _01 shouldBe a[java.lang.Integer]
    _02 shouldBe a[java.lang.Integer]
    _03 shouldBe a[java.lang.Integer]

    _01 should be(42)
    _02 should be(42 * 2)
    _03 should be(42 * 2 * 3)
  }

  it should "be able to pass through a JSON document" in {

    val context = js.newContext()

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

    context.putJson("obj", node)

    context.getJson("obj") should be(Some(node))
  }
}
