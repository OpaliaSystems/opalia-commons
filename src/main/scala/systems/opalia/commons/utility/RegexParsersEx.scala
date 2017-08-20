package systems.opalia.commons.utility

import scala.util.parsing.combinator._


trait RegexParsersEx
  extends RegexParsers {

  def repNM[T](n: Int, m: Int, p: => Parser[T]): Parser[List[T]] =
    if (m <= 0)
      success(Nil)
    else if (n > 0)
      p ~ repNM(n - 1, m - 1, p) ^^ { case ~(x, xs) => x :: xs }
    else
      p ~ repNM(n - 1, m - 1, p) ^^ { case ~(x, xs) => x :: xs } | success(Nil)
}
