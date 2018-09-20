package systems.opalia.commons.net

import java.net.InetAddress
import java.util.Objects
import systems.opalia.commons.utility.RegexParsersEx
import systems.opalia.interfaces.rendering._


sealed trait IpAddress
  extends StringRenderable
    with ByteRenderable {

  protected val segments: Vector[Byte]

  val representation: IpAddress.Representation

  override def equals(that: Any): Boolean =
    that match {

      case that: IpAddress if (this.segments.sameElements(that.segments)) => true
      case _ => false
    }

  def equalsSyntactically(that: Any): Boolean =
    that match {

      case that: IpAddress if (this.toString == that.toString) => true
      case _ => false
    }

  override def hashCode: Int =
    Objects.hash(segments.map(Byte.box): _*)

  def getInetAddress: InetAddress =
    if (representation.style == IpAddress.Style.V4)
      InetAddress.getByAddress(toBytes().takeRight(4).toArray)
    else
      InetAddress.getByAddress(toBytes().toArray)

  def renderString(renderer: StringRenderer): StringRenderer = {

    def mkIPv4Part(segments: Vector[Byte]): StringRenderer =
      renderer.newEmpty.glue(segments.map(mkIPv4Segment), ".")

    def mkIPv6Part(segments: Vector[Byte]): StringRenderer =
      renderer.newEmpty.glue(segments.sliding(2, 2).toVector.map(mkIPv6Segment), ":")

    def mkIPv6Segment(bytes: Vector[Byte]): StringRenderer = {

      val value = ((bytes(0) << 8) & 0xFF00) | (bytes(1) & 0x00FF)

      if (representation.leadingZero)
        renderer.newEmpty ~ "%04x".format(value)
      else
        renderer.newEmpty ~ "%x".format(value)
    }

    def mkIPv4Segment(byte: Byte): StringRenderer =
      renderer.newEmpty ~ "%d".format(byte & 0xFF)

    def trimRight(segments: Vector[Byte]): Vector[Byte] =
      segments
        .take(representation.nullIndex * 2)
        .reverse
        .sliding(2, 2).toVector
        .dropWhile(x => x(0) == 0x00 && x(1) == 0x00)
        .flatten
        .reverse

    def trimLeft(segments: Vector[Byte]): Vector[Byte] =
      segments
        .drop(representation.nullIndex * 2)
        .sliding(2, 2).toVector
        .dropWhile(x => x(0) == 0x00 && x(1) == 0x00)
        .flatten

    representation.style match {

      case IpAddress.Style.V4 => {

        renderer ~ mkIPv4Part(segments.takeRight(4))
      }

      case IpAddress.Style.V4InV6 => {

        if (representation.nullIndex == -1)
          renderer ~ mkIPv6Part(segments.dropRight(4)) ~ ':' ~ mkIPv4Part(segments.takeRight(4))
        else {

          val left = trimRight(segments.dropRight(4))
          val right = trimLeft(segments.dropRight(4))

          renderer ~ mkIPv6Part(left) ~ "::" ~
            (if (right.isEmpty) renderer.newEmpty else mkIPv6Part(right) ~ ':') ~
            mkIPv4Part(segments.takeRight(4))
        }
      }

      case IpAddress.Style.V6 => {

        if (representation.nullIndex == -1)
          renderer ~ mkIPv6Part(segments)
        else
          renderer ~ mkIPv6Part(trimRight(segments)) ~ "::" ~ mkIPv6Part(trimLeft(segments))
      }

    }
  }

  def renderBytes(renderer: ByteRenderer): ByteRenderer = {

    renderer ++= segments
  }
}

object IpAddress {

  def apply(segments: Iterable[Byte]): IpAddress =
    apply(segments, Representation())

  def apply(segments: Iterable[Byte], representation: Representation): IpAddress = {

    val xs = segments.toVector

    val _segments =
      if (representation.style == IpAddress.Style.V4 && xs.length >= 4 && xs.length <= 16 && xs.length % 2 == 0) {

        if (representation.nullIndex < -1 || representation.nullIndex > 5)
          throw new IndexOutOfBoundsException("Incorrect position of consecutive null index.")

        val prefix = (Vector.fill(10)(0x00) ++ Vector.fill(2)(0xFF)).map(_.toByte)
        val result = prefix.drop(xs.length - 4) ++ xs

        if (!result.startsWith(prefix))
          throw new IndexOutOfBoundsException("Incorrect byte of sequence.")

        result

      } else if (representation.style != IpAddress.Style.V4 && xs.length == 16) {

        if (representation.nullIndex != -1)
          throw new IllegalArgumentException("Incorrect position of consecutive null index.")

        xs

      } else if (representation.style != IpAddress.Style.V4 && xs.length < 16 && xs.length % 2 == 0) {

        if (representation.nullIndex < 0 || representation.nullIndex > 7 || representation.nullIndex > xs.length)
          throw new IndexOutOfBoundsException("Incorrect position of consecutive null index.")

        xs.take(representation.nullIndex * 2) ++
          Vector.fill[Byte](16 - xs.length)(0x00) ++
          xs.takeRight(xs.length - representation.nullIndex * 2)

      } else
        throw new IllegalArgumentException("Incorrect length of segment vector.")

    val _representation = representation

    new IpAddress {

      val segments = _segments
      val representation = _representation
    }
  }

  def apply(address: String): IpAddress =
    Parser.parse(address)

  private object Parser
    extends RegexParsersEx
      with AbstractParser {

    def expression: Parser[IpAddress] =
      `IPv4-ADR` | `IPv6-ADR`

    def parse(value: String): IpAddress =
      parseAll(expression, value) match {
        case Success(result, _) => result
        case failure: NoSuccess => throw new IllegalArgumentException(failure.msg)
      }
  }

  case class Representation(nullIndex: Int = -1, leadingZero: Boolean = true, style: Style.Value = Style.V6)

  object Style
    extends Enumeration {

    val V4, V4InV6, V6 = Value
  }

  trait AbstractParser {
    self: RegexParsersEx =>

    /*

      IPv4-SEG

     */

    def `IPv4-SEG`: Parser[Byte] =
      """(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])""".r ^^ (_.toInt.toByte)

    /*

      IPv4-ADR = IPv4-SEG '.' IPv4-SEG '.' IPv4-SEG '.' IPv4-SEG

     */

    def `IPv4-ADR-BYTES`: Parser[Vector[Byte]] =
      `IPv4-SEG` ~ "." ~ `IPv4-SEG` ~ "." ~ `IPv4-SEG` ~ "." ~ `IPv4-SEG` ^^ {
        case a ~ _ ~ b ~ _ ~ c ~ _ ~ d => Vector(a, b, c, d)
      }

    def `IPv4-ADR`: Parser[IpAddress] =
      `IPv4-ADR-BYTES` ^^ (x => IpAddress(x, IpAddress.Representation(nullIndex = 0, style = Style.V4)))

    /*

      IPv6-SEG

     */

    def `IPv6-SEG`: Parser[(Vector[Byte], Boolean)] =
      """([a-fA-F0-9]{1,4})""".r ^^ {
        x =>

          val value = Integer.parseInt(x, 16)
          val leadingZero = x.length > 1 && x.charAt(0) == '0'

          (Vector(((value >> 8) & 0xFF).toByte, (value & 0xFF).toByte), leadingZero)
      }

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

    def `IPv6-ADR`: Parser[IpAddress] =
      repNM(1, 1, `IPv6-SEG` <~ ":") ~ ":" ~ repNM(1, 4, `IPv6-SEG` <~ ":") ~ `IPv4-ADR-BYTES` ^^ {
        case xs ~ _ ~ ys ~ z =>

          IpAddress(
            xs.flatMap(_._1) ++ ys.flatMap(_._1) ++ z,
            IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V4InV6)
          )
      } |
        repNM(1, 2, `IPv6-SEG` <~ ":") ~ ":" ~ repNM(1, 3, `IPv6-SEG` <~ ":") ~ `IPv4-ADR-BYTES` ^^ {
          case xs ~ _ ~ ys ~ z =>

            IpAddress(
              xs.flatMap(_._1) ++ ys.flatMap(_._1) ++ z,
              IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V4InV6)
            )
        } |
        repNM(1, 3, `IPv6-SEG` <~ ":") ~ ":" ~ repNM(1, 2, `IPv6-SEG` <~ ":") ~ `IPv4-ADR-BYTES` ^^ {
          case xs ~ _ ~ ys ~ z =>

            IpAddress(
              xs.flatMap(_._1) ++ ys.flatMap(_._1) ++ z,
              IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V4InV6)
            )
        } |
        repNM(1, 4, `IPv6-SEG` <~ ":") ~ ":" ~ repNM(1, 1, `IPv6-SEG` <~ ":") ~ `IPv4-ADR-BYTES` ^^ {
          case xs ~ _ ~ ys ~ z =>

            IpAddress(
              xs.flatMap(_._1) ++ ys.flatMap(_._1) ++ z,
              IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V4InV6)
            )
        } |
        repNM(1, 5, `IPv6-SEG` <~ ":") ~ ":" ~ `IPv4-ADR-BYTES` ^^ {
          case xs ~ _ ~ z =>

            IpAddress(
              xs.flatMap(_._1) ++ z,
              IpAddress.Representation(xs.length, xs.exists(_._2), Style.V4InV6)
            )
        } |
        repNM(6, 6, `IPv6-SEG` <~ ":") ~ `IPv4-ADR-BYTES` ^^ {
          case xs ~ z =>

            IpAddress(
              xs.flatMap(_._1) ++ z,
              IpAddress.Representation(-1, xs.exists(_._2), Style.V4InV6)
            )
        } |
        "::" ~ repNM(0, 5, `IPv6-SEG` <~ ":") ~ `IPv4-ADR-BYTES` ^^ {
          case _ ~ ys ~ z =>

            IpAddress(
              ys.flatMap(_._1) ++ z,
              IpAddress.Representation(0, ys.exists(_._2), Style.V4InV6)
            )
        } |
        repNM(1, 1, `IPv6-SEG` <~ ":") ~ repNM(1, 6, ":" ~> `IPv6-SEG`) ^^ {
          case xs ~ ys =>

            IpAddress(
              xs.flatMap(_._1) ++ ys.flatMap(_._1),
              IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V6)
            )
        } |
        repNM(1, 2, `IPv6-SEG` <~ ":") ~ repNM(1, 5, ":" ~> `IPv6-SEG`) ^^ {
          case xs ~ ys =>

            IpAddress(
              xs.flatMap(_._1) ++ ys.flatMap(_._1),
              IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V6)
            )
        } |
        repNM(1, 3, `IPv6-SEG` <~ ":") ~ repNM(1, 4, ":" ~> `IPv6-SEG`) ^^ {
          case xs ~ ys =>

            IpAddress(
              xs.flatMap(_._1) ++ ys.flatMap(_._1),
              IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V6)
            )
        } |
        repNM(1, 4, `IPv6-SEG` <~ ":") ~ repNM(1, 3, ":" ~> `IPv6-SEG`) ^^ {
          case xs ~ ys =>

            IpAddress(
              xs.flatMap(_._1) ++ ys.flatMap(_._1),
              IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V6)
            )
        } |
        repNM(1, 5, `IPv6-SEG` <~ ":") ~ repNM(1, 2, ":" ~> `IPv6-SEG`) ^^ {
          case xs ~ ys =>

            IpAddress(
              xs.flatMap(_._1) ++ ys.flatMap(_._1),
              IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V6)
            )
        } |
        repNM(1, 6, `IPv6-SEG` <~ ":") ~ repNM(1, 1, ":" ~> `IPv6-SEG`) ^^ {
          case xs ~ ys =>

            IpAddress(
              xs.flatMap(_._1) ++ ys.flatMap(_._1),
              IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V6)
            )
        } |
        repNM(1, 7, `IPv6-SEG` <~ ":") ~ ":" ^^ {
          case xs ~ _ =>

            IpAddress(
              xs.flatMap(_._1),
              IpAddress.Representation(xs.length, xs.exists(_._2), Style.V6)
            )
        } |
        repNM(7, 7, `IPv6-SEG` <~ ":") ~ `IPv6-SEG` ^^ {
          case xs ~ x =>

            IpAddress(
              xs.flatMap(_._1) ++ x._1,
              IpAddress.Representation(-1, (xs :+ x).exists(_._2), Style.V6)
            )
        } |
        ":" ~ repNM(1, 7, ":" ~> `IPv6-SEG`) ^^ {
          case _ ~ ys =>

            IpAddress(
              ys.flatMap(_._1),
              IpAddress.Representation(0, ys.exists(_._2), Style.V6)
            )
        } |
        "::" ^^ {
          case _ =>

            IpAddress(
              Nil,
              IpAddress.Representation(nullIndex = 0, style = Style.V6)
            )
        }

    /*

    IPv6-LITERAL = '[' IPv6-ADR ']'

     */

    def `IPv6-LITERAL`: Parser[IpAddress] =
      "[" ~> `IPv6-ADR` <~ "]"
  }

}
