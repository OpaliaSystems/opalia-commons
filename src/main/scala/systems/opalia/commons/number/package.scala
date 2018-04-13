package systems.opalia.commons


package object number {

  implicit class StringToNumberImprovements(string: String) {

    import scala.util.control.Exception._

    def toByteOpt: Option[Byte] =
      catching(classOf[NumberFormatException]) opt string.toByte

    def toShortOpt: Option[Short] =
      catching(classOf[NumberFormatException]) opt string.toShort

    def toIntOpt: Option[Int] =
      catching(classOf[NumberFormatException]) opt string.toInt

    def toLongOpt: Option[Long] =
      catching(classOf[NumberFormatException]) opt string.toLong

    def toFloatOpt: Option[Float] =
      catching(classOf[NumberFormatException]) opt string.toFloat

    def toDoubleOpt: Option[Double] =
      catching(classOf[NumberFormatException]) opt string.toDouble

    def toBigInt: BigInt =
      BigInt(string)

    def toBigIntOpt: Option[BigInt] =
      catching(classOf[NumberFormatException]) opt BigInt(string)

    def toBigDecimal: BigDecimal =
      BigDecimal(string)

    def toBigDecimalOpt: Option[BigDecimal] =
      catching(classOf[NumberFormatException]) opt BigDecimal(string)

    def toByteX: Byte =
      NumberUtils.hex2Long(string, java.lang.Byte.BYTES).toByte

    def toShortX: Short =
      NumberUtils.hex2Long(string, java.lang.Short.BYTES).toShort

    def toIntX: Int =
      NumberUtils.hex2Long(string, java.lang.Integer.BYTES).toInt

    def toLongX: Long =
      NumberUtils.hex2Long(string, java.lang.Long.BYTES)

    def toByteXOpt: Option[Byte] =
      catching(classOf[NumberFormatException]) opt NumberUtils.hex2Long(string, java.lang.Byte.BYTES).toByte

    def toShortXOpt: Option[Short] =
      catching(classOf[NumberFormatException]) opt NumberUtils.hex2Long(string, java.lang.Short.BYTES).toShort

    def toIntXOpt: Option[Int] =
      catching(classOf[NumberFormatException]) opt NumberUtils.hex2Long(string, java.lang.Integer.BYTES).toInt

    def toLongXOpt: Option[Long] =
      catching(classOf[NumberFormatException]) opt NumberUtils.hex2Long(string, java.lang.Long.BYTES)

    def toStrictBoolean: Boolean =
      if (string.toLowerCase == "true" || string.toLowerCase == "on" || string.toLowerCase == "yes")
        true
      else if (string.toLowerCase == "false" || string.toLowerCase == "off" || string.toLowerCase == "no")
        false
      else
        throw new NumberFormatException(s"Cannot get boolean from string: $string")

    def toStrictBooleanOpt: Option[Boolean] =
      catching(classOf[NumberFormatException]) opt string.toStrictBoolean
  }

}
