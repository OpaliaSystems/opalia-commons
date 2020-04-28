package systems.opalia.commons.json.ref

import org.apache.commons.text.StringEscapeUtils
import org.scalatest._
import play.api.libs.json.Json


class JsonPointerTest
  extends FlatSpec
    with Matchers {

  it should "parse JSON pointers correctly" in {

    val testNode =
      Json.parse(
        """
          |{
          |  "foo": ["bar", "baz"],
          |  "bla": {"blubb": 42},
          |  "": 0,
          |  " ": 1,
          |  "a/b": 2,
          |  "m~n": 3,
          |  "c%d": 4,
          |  "e^f": 5,
          |  "g|h": 6,
          |  "i\\j": 7,
          |  "k\"l": 8,
          |  "0" : 9,
          |  "1" : 10,
          |  "€": 11
          |}
      """.stripMargin.trim)

    def unescape(value: String): String =
      StringEscapeUtils.unescapeEcmaScript(value)

    JsonPointer(unescape("")).search(testNode) shouldBe testNode
    JsonPointer(unescape("""/foo""")).search(testNode) shouldBe testNode("foo")
    JsonPointer(unescape("""/foo/0""")).search(testNode) shouldBe testNode("foo")(0)
    JsonPointer(unescape("""/foo/1""")).search(testNode) shouldBe testNode("foo")(1)
    JsonPointer(unescape("""/bla/blubb""")).search(testNode) shouldBe testNode("bla")("blubb")
    JsonPointer(unescape("/")).search(testNode) shouldBe testNode("")
    JsonPointer(unescape("/ ")).search(testNode) shouldBe testNode(" ")
    JsonPointer(unescape("""/a~1b""")).search(testNode) shouldBe testNode("""a/b""")
    JsonPointer(unescape("""/m~0n""")).search(testNode) shouldBe testNode("""m~n""")
    JsonPointer(unescape("""/c%d""")).search(testNode) shouldBe testNode("""c%d""")
    JsonPointer(unescape("""/e^f""")).search(testNode) shouldBe testNode("""e^f""")
    JsonPointer(unescape("""/g|h""")).search(testNode) shouldBe testNode("""g|h""")
    JsonPointer(unescape("""/i\\j""")).search(testNode) shouldBe testNode("""i\j""")
    JsonPointer(unescape("""/k\"l""")).search(testNode) shouldBe testNode("""k"l""")
    JsonPointer(unescape("/0")).search(testNode) shouldBe testNode("0")
    JsonPointer(unescape("/1")).search(testNode) shouldBe testNode("1")
    JsonPointer(unescape("/€")).search(testNode) shouldBe testNode("€")
  }
}
