package systems.opalia.commons.json.format

import play.api.libs.json._
import systems.opalia.commons.identifier.UniversallyUniqueId


class UniversallyUniqueIdFormat
  extends Format[UniversallyUniqueId] {

  override def reads(node: JsValue): JsResult[UniversallyUniqueId] =
    node match {
      case JsString(x) =>
        UniversallyUniqueId.getFromOpt(x) match {
          case Some(v) => JsSuccess(v)
          case None => JsError("error.expected.format.UniversallyUniqueId (UUID)")
        }
      case _ => JsError("error.expected.jsstring")
    }

  def writes(x: UniversallyUniqueId): JsValue =
    JsString(x.toString)
}
