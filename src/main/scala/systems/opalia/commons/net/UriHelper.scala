package systems.opalia.commons.net

import systems.opalia.interfaces.rendering.Renderer


object UriHelper {

  object Chars {

    val numeric = "1234567890"
    val alphabeticLower = "abcdefghijklmnopqrstuvwxyz"
    val alphabeticUpper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val alphabetic = alphabeticLower + alphabeticUpper
    val alphanumeric = alphabetic + numeric
    val hexadecimal = numeric + alphabeticLower.take(6) + alphabeticUpper.take(6)
    val unreserved = alphanumeric + "-._~"
    val gendelims = ":/?#[]@"
    val subdelims = "!$&'()*+,;="
    val reserved = gendelims + subdelims
  }

  def encode(data: String,
             keepChars: String,
             replaceSpaces: Boolean = false,
             charset: String = Renderer.defaultCharset): String = {

    def encode(chars: List[Char], acc: List[Char]): String =
      chars match {
        case ' ' :: rest if (replaceSpaces) =>
          encode(rest, acc :+ '+')
        case x :: rest =>
          if (keepChars.contains(x))
            encode(rest, acc :+ x)
          else
            encode(rest, acc ++ x.toString
              .getBytes(charset)
              .map("%02X" format _).map('%' + _).mkString)
        case Nil =>
          new String(acc.toArray)
      }

    encode(data.toList, Nil)
  }

  def decode(data: String,
             replaceSpaces: Boolean = false,
             charset: String = Renderer.defaultCharset): String = {

    def decode(chars: List[Char], acc: List[Byte]): String =
      chars match {
        case '%' :: x :: y :: rest if (Chars.hexadecimal.contains(x) && Chars.hexadecimal.contains(y)) =>
          decode(rest, acc :+ Integer.parseInt(String.copyValueOf(Array(x, y)), 16).toByte)
        case '+' :: rest if (replaceSpaces) =>
          decode(rest, acc ++ ' '.toString.getBytes(charset))
        case x :: rest =>
          decode(rest, acc ++ x.toString.getBytes(charset))
        case Nil =>
          new String(acc.toArray, charset)
      }

    decode(data.toList, Nil)
  }
}
