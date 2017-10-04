package systems.opalia.commons.json.format

import play.api.libs.json._
import systems.opalia.commons.identifier.ObjectId


class ObjectIdFormat
  extends Format[ObjectId] {

  override def reads(node: JsValue): JsResult[ObjectId] =
    node match {
      case JsString(x) =>
        ObjectId.getFromOpt(x) match {
          case Some(v) => JsSuccess(v)
          case None => JsError("error.expected.format.ObjectId")
        }
      case _ => JsError("error.expected.jsstring")
    }

  def writes(x: ObjectId): JsValue =
    JsString(x.toString)
}
