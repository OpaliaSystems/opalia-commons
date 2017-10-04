package systems.opalia.commons.json.format

import play.api.libs.json._


class BigIntFormat
  extends Format[BigInt] {

  override def reads(node: JsValue): JsResult[BigInt] =
    node match {
      case JsNumber(x) =>
        if (x.isWhole())
          JsSuccess(x.toBigInt())
        else
          JsError("error.expected.format.BigInt (integer)")
      case _ => JsError("error.expected.jsnumber")
    }

  def writes(x: BigInt): JsValue =
    JsNumber(BigDecimal(x))
}
