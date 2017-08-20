package systems.opalia.commons.net

import systems.opalia.commons.utility.RegexParsersEx


private[net] trait IpAndPortParser {
  self: RegexParsersEx =>

  /*

    IPv4-SEG

   */

  def `IPv4-SEG`: Parser[String] =
    """(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])""".r

  /*

    IPv4-ADR = IPv4-SEG '.' IPv4-SEG '.' IPv4-SEG '.' IPv4-SEG

   */

  def `IPv4-ADR`: Parser[String] =
    `IPv4-SEG` ~ "." ~ `IPv4-SEG` ~ "." ~ `IPv4-SEG` ~ "." ~ `IPv4-SEG` ^^ {
      case a ~ _ ~ b ~ _ ~ c ~ _ ~ d => a + "." + b + "." + c + "." + d
    }

  /*

    IPv6-SEG

   */

  def `IPv6-SEG`: Parser[String] =
    """([a-fA-F0-9]{1,4})""".r

  /*

    IPv6-ADR = (
        (IPv6-SEG ':'){1,1} ':' (IPv6-SEG ':'){1,4} IPv4-ADR  # 1::3:4:5:6:x.x.x.x      1::6:x.x.x.x
      | (IPv6-SEG ':'){1,2} ':' (IPv6-SEG ':'){1,3} IPv4-ADR  # 1::4:5:6:x.x.x.x        1:2::6:x.x.x.x
      | (IPv6-SEG ':'){1,3} ':' (IPv6-SEG ':'){1,2} IPv4-ADR  # 1::5:6:x.x.x.x          1:2:3::6:x.x.x.x
      | (IPv6-SEG ':'){1,4} ':' (IPv6-SEG ':'){1,1} IPv4-ADR  # 1::6:x.x.x.x            1:2:3:4::6:x.x.x.x
      | (IPv6-SEG ':'){1,5} ':'                     IPv4-ADR  # 1::x.x.x.x              1:2:3:4:5::x.x.x.x
      | (IPv6-SEG ':'){6,6}                         IPv4-ADR  # 1:2:3:4:5:6:x.x.x.x
      |           ':'       ':' (IPv6-SEG ':'){0,5} IPv4-ADR  # ::2:3:4:5:6:x.x.x.x     ::x.x.x.x

      | (IPv6-SEG ':'){1,1} (':' IPv6-SEG){1,6}               # 1::3:4:5:6:7:8          1::8
      | (IPv6-SEG ':'){1,2} (':' IPv6-SEG){1,5}               # 1::4:5:6:7:8            1:2::8
      | (IPv6-SEG ':'){1,3} (':' IPv6-SEG){1,4}               # 1::5:6:7:8              1:2:3::8
      | (IPv6-SEG ':'){1,4} (':' IPv6-SEG){1,3}               # 1::6:7:8                1:2:3:4::8
      | (IPv6-SEG ':'){1,5} (':' IPv6-SEG){1,2}               # 1::7:8                  1:2:3:4:5::8
      | (IPv6-SEG ':'){1,6} (':' IPv6-SEG){1,1}               # 1::8                    1:2:3:4:5:6::8
      | (IPv6-SEG ':'){1,7}  ':'                              # 1::                     1:2:3:4:5:6:7::
      | (IPv6-SEG ':'){7,7}      IPv6-SEG                     # 1:2:3:4:5:6:7:8
      |           ':'       (':' IPv6-SEG){1,7}               # ::2:3:4:5:6:7:8         ::8

      | ':' ':'                                               # ::
    )

   */

  def `IPv6-ADR`: Parser[String] =
    repNM(1, 1, `IPv6-SEG` <~ ":") ~ ":" ~ repNM(1, 4, `IPv6-SEG` <~ ":") ~ `IPv4-ADR` ^^ {
      case xs ~ _ ~ ys ~ z => xs.map(_ + ":").mkString + ":" + ys.map(_ + ":").mkString + z
    } |
      repNM(1, 2, `IPv6-SEG` <~ ":") ~ ":" ~ repNM(1, 3, `IPv6-SEG` <~ ":") ~ `IPv4-ADR` ^^ {
        case xs ~ _ ~ ys ~ z => xs.map(_ + ":").mkString + ":" + ys.map(_ + ":").mkString + z
      } |
      repNM(1, 3, `IPv6-SEG` <~ ":") ~ ":" ~ repNM(1, 2, `IPv6-SEG` <~ ":") ~ `IPv4-ADR` ^^ {
        case xs ~ _ ~ ys ~ z => xs.map(_ + ":").mkString + ":" + ys.map(_ + ":").mkString + z
      } |
      repNM(1, 4, `IPv6-SEG` <~ ":") ~ ":" ~ repNM(1, 1, `IPv6-SEG` <~ ":") ~ `IPv4-ADR` ^^ {
        case xs ~ _ ~ ys ~ z => xs.map(_ + ":").mkString + ":" + ys.map(_ + ":").mkString + z
      } |
      repNM(1, 5, `IPv6-SEG` <~ ":") ~ ":" ~ `IPv4-ADR` ^^ {
        case xs ~ _ ~ z => xs.map(_ + ":").mkString + ":" + z
      } |
      repNM(6, 6, `IPv6-SEG` <~ ":") ~ `IPv4-ADR` ^^ {
        case xs ~ z => xs.map(_ + ":").mkString + z
      } |
      "::" ~ repNM(0, 5, `IPv6-SEG` <~ ":") ~ `IPv4-ADR` ^^ {
        case _ ~ ys ~ z => "::" + ys.map(_ + ":").mkString + z
      } |
      repNM(1, 1, `IPv6-SEG` <~ ":") ~ repNM(1, 6, ":" ~> `IPv6-SEG`) ^^ {
        case xs ~ ys => xs.map(_ + ":").mkString + ys.map(":" + _).mkString
      } |
      repNM(1, 2, `IPv6-SEG` <~ ":") ~ repNM(1, 5, ":" ~> `IPv6-SEG`) ^^ {
        case xs ~ ys => xs.map(_ + ":").mkString + ys.map(":" + _).mkString
      } |
      repNM(1, 3, `IPv6-SEG` <~ ":") ~ repNM(1, 4, ":" ~> `IPv6-SEG`) ^^ {
        case xs ~ ys => xs.map(_ + ":").mkString + ys.map(":" + _).mkString
      } |
      repNM(1, 4, `IPv6-SEG` <~ ":") ~ repNM(1, 3, ":" ~> `IPv6-SEG`) ^^ {
        case xs ~ ys => xs.map(_ + ":").mkString + ys.map(":" + _).mkString
      } |
      repNM(1, 5, `IPv6-SEG` <~ ":") ~ repNM(1, 2, ":" ~> `IPv6-SEG`) ^^ {
        case xs ~ ys => xs.map(_ + ":").mkString + ys.map(":" + _).mkString
      } |
      repNM(1, 6, `IPv6-SEG` <~ ":") ~ repNM(1, 1, ":" ~> `IPv6-SEG`) ^^ {
        case xs ~ ys => xs.map(_ + ":").mkString + ys.map(":" + _).mkString
      } |
      repNM(1, 7, `IPv6-SEG` <~ ":") ~ ":" ^^ {
        case xs ~ _ => xs.map(_ + ":").mkString + ":"
      } |
      repNM(7, 7, `IPv6-SEG` <~ ":") ~ `IPv6-SEG` ^^ {
        case xs ~ x => xs.map(_ + ":").mkString + x
      } |
      ":" ~ repNM(1, 7, ":" ~> `IPv6-SEG`) ^^ {
        case _ ~ ys => ":" + ys.map(":" + _).mkString
      } |
      "::"

  /*

  IPv6-LITERAL = '[' IPv6-ADR ']'

   */

  def `IPv6-LITERAL`: Parser[String] =
    "[" ~> `IPv6-ADR` <~ "]"

  /*

  PORT

   */

  def `PORT`: Parser[Int] =
    """([1-9][0-9]*)""".r ^^ (_.toInt) ^^ {
      x =>

        if (x > 65535)
          throw new IllegalArgumentException(
            s"The highest port number is 65535 but $x found.")

        x
    }
}
