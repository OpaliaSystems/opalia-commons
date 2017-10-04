package systems.opalia.commons.json.format

import play.api.libs.json._
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}


class RegexFormat
  extends Format[Regex] {

  override def reads(node: JsValue): JsResult[Regex] =
    node match {
      case JsString(x) =>
        Try(x.r) match {
          case Success(v) => JsSuccess(v)
          case Failure(e) => JsError("error.expected.format.Regex (regular expression)\n" + e.getMessage)
        }
      case _ => JsError("error.expected.jsstring")
    }

  def writes(x: Regex): JsValue =
    JsString(x.regex)
}
