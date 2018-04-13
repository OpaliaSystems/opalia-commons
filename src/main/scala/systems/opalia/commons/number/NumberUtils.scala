package systems.opalia.commons.number


object NumberUtils {

  def getNumericValue(char: Char, radix: Int): Int = {

    val value = Character.getNumericValue(char)

    if (radix < 2 || radix > 36 || value < 0 || value >= radix)
      throw new NumberFormatException("Value out of range.")

    value
  }

  def hex2Long(chars: Iterable[Char], size: Int): Long = {

    if (chars.isEmpty)
      throw new NumberFormatException("Expect non empty string.")

    if (size <= 0 || size * 2 < chars.size)
      throw new NumberFormatException("Value out of range.")

    var value = getNumericValue(chars.head, 16)

    chars.tail.foreach {
      char =>

        value = (value << 4) + getNumericValue(char, 16)
    }

    value
  }
}
