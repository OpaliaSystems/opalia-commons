package systems.opalia.commons.json

import play.api.libs.json._
import scala.collection.immutable.ListMap
import systems.opalia.interfaces.json.JsonAst


object JsonAstTransformer {

  def toPlayJson(json: JsonAst.JsonValue): JsValue =
    json match {

      case JsonAst.JsonNull =>
        JsNull

      case JsonAst.JsonBoolean(x) =>
        JsBoolean(x)

      case JsonAst.JsonString(x) =>
        JsString(x)

      case x: JsonAst.JsonNumber =>
        JsNumber(x.bigDecimalValue)

      case x: JsonAst.JsonArray =>
        toPlayJsonArray(x)

      case x: JsonAst.JsonObject =>
        toPlayJsonObject(x)
    }

  def fromPlayJson(json: JsValue): JsonAst.JsonValue =
    json match {

      case JsNull =>
        JsonAst.JsonNull

      case JsBoolean(x) =>
        JsonAst.JsonBoolean(x)

      case JsString(x) =>
        JsonAst.JsonString(x)

      case JsNumber(x) =>
        JsonAst.JsonNumberBigDecimal(x)

      case x: JsArray =>
        fromPlayJsonArray(x)

      case x: JsObject =>
        fromPlayJsonObject(x)
    }

  def toPlayJsonArray(json: JsonAst.JsonArray): JsArray =
    JsArray(json.elements.map(x => toPlayJson(x)))

  def fromPlayJsonArray(json: JsArray): JsonAst.JsonArray =
    JsonAst.JsonArray(json.value.map(x => fromPlayJson(x)))

  def toPlayJsonObject(json: JsonAst.JsonObject): JsObject =
    JsObject(json.fields.map(x => x._1 -> toPlayJson(x._2)))

  def fromPlayJsonObject(json: JsObject): JsonAst.JsonObject =
    JsonAst.JsonObject(ListMap(json.fields.map(x => x._1 -> fromPlayJson(x._2)): _*))
}
