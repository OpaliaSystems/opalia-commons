package systems.opalia.commons.net

import java.net.InetAddress
import java.util.Objects
import org.parboiled2._
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
    new IpAddressParser(address).`ip-adr-expression`.run().get

  private class IpAddressParser(val input: ParserInput)
    extends Parser
      with AbstractParser {

    def `ip-adr-expression`: Rule1[IpAddress] =
      rule {

        `ip-adr` ~ EOI
      }
  }

  case class Representation(nullIndex: Int = -1, leadingZero: Boolean = true, style: Style = Style.V6)

  sealed trait Style

  object Style {

    case object V4
      extends Style

    case object V4InV6
      extends Style

    case object V6
      extends Style

  }

  private[net] trait AbstractParser {
    self: Parser =>

    private val Digit04 = CharPredicate('0' to '4')
    private val Digit05 = CharPredicate('0' to '5')

    /*

      ipv4-seg

     */

    def `ipv4-seg`: Rule1[Byte] =
      rule {

        capture(
          '2' ~ ('5' ~ Digit05 | Digit04 ~ CharPredicate.Digit)
            | '1' ~ CharPredicate.Digit ~ CharPredicate.Digit
            | CharPredicate.Digit19 ~ CharPredicate.Digit
            | CharPredicate.Digit) ~> ((x: String) => x.toInt.toByte)
      }

    /*

      ipv4-adr = ipv4-seg '.' ipv4-seg '.' ipv4-seg '.' ipv4-seg

     */

    def `ipv4-adr`: Rule1[IpAddress] =
      rule {

        `ipv4-adr-bytes` ~> {
          (x: Vector[Byte]) =>

            IpAddress(x, IpAddress.Representation(nullIndex = 0, style = Style.V4))
        }
      }

    def `ipv4-adr-bytes`: Rule1[Vector[Byte]] =
      rule {

        `ipv4-seg` ~ '.' ~ `ipv4-seg` ~ '.' ~ `ipv4-seg` ~ '.' ~ `ipv4-seg` ~> {
          (a: Byte, b: Byte, c: Byte, d: Byte) =>

            Vector(a, b, c, d)
        }
      }

    /*

      ipv6-seg

     */

    def `ipv6-seg`: Rule1[(Vector[Byte], Boolean)] =
      rule {

        capture(CharPredicate.HexDigit ~ CharPredicate.HexDigit ~ CharPredicate.HexDigit ~ CharPredicate.HexDigit |
          CharPredicate.HexDigit ~ CharPredicate.HexDigit ~ CharPredicate.HexDigit |
          CharPredicate.HexDigit ~ CharPredicate.HexDigit |
          CharPredicate.HexDigit) ~> {
          (x: String) =>

            val value = Integer.parseInt(x, 16)
            val leadingZero = x.length > 1 && x.charAt(0) == '0'

            (Vector(((value >> 8) & 0xFF).toByte, (value & 0xFF).toByte), leadingZero)
        }
      }

    /*

      ipv6-adr = (
          (ipv6-seg ':'){1,1} ':' (ipv6-seg ':'){1,4} ipv4-adr  # 1::3:4:5:6:x.x.x.x      1::6:x.x.x.x
        | (ipv6-seg ':'){1,2} ':' (ipv6-seg ':'){1,3} ipv4-adr  # 1::4:5:6:x.x.x.x        1:2::6:x.x.x.x
        | (ipv6-seg ':'){1,3} ':' (ipv6-seg ':'){1,2} ipv4-adr  # 1::5:6:x.x.x.x          1:2:3::6:x.x.x.x
        | (ipv6-seg ':'){1,4} ':' (ipv6-seg ':'){1,1} ipv4-adr  # 1::6:x.x.x.x            1:2:3:4::6:x.x.x.x
        | (ipv6-seg ':'){1,5} ':'                     ipv4-adr  # 1::x.x.x.x              1:2:3:4:5::x.x.x.x
        | (ipv6-seg ':'){6,6}                         ipv4-adr  # 1:2:3:4:5:6:x.x.x.x
        |           ':'       ':' (ipv6-seg ':'){0,5} ipv4-adr  # ::2:3:4:5:6:x.x.x.x     ::x.x.x.x

        | (ipv6-seg ':'){1,1} (':' ipv6-seg){1,6}               # 1::3:4:5:6:7:8          1::8
        | (ipv6-seg ':'){1,2} (':' ipv6-seg){1,5}               # 1::4:5:6:7:8            1:2::8
        | (ipv6-seg ':'){1,3} (':' ipv6-seg){1,4}               # 1::5:6:7:8              1:2:3::8
        | (ipv6-seg ':'){1,4} (':' ipv6-seg){1,3}               # 1::6:7:8                1:2:3:4::8
        | (ipv6-seg ':'){1,5} (':' ipv6-seg){1,2}               # 1::7:8                  1:2:3:4:5::8
        | (ipv6-seg ':'){1,6} (':' ipv6-seg){1,1}               # 1::8                    1:2:3:4:5:6::8
        | (ipv6-seg ':'){1,7}  ':'                              # 1::                     1:2:3:4:5:6:7::
        | (ipv6-seg ':'){7,7}      ipv6-seg                     # 1:2:3:4:5:6:7:8
        |           ':'       (':' ipv6-seg){1,7}               # ::2:3:4:5:6:7:8         ::8

        | ':' ':'                                               # ::
      )

     */

    def `ipv6-adr`: Rule1[IpAddress] =
      rule {

        (1 to 1).times(`ipv6-seg` ~ ':') ~ ':' ~ (1 to 4).times(`ipv6-seg` ~ ':') ~ `ipv4-adr-bytes` ~> {
          (xs, ys, z) =>

            IpAddress(
              xs.flatMap(_._1) ++ ys.flatMap(_._1) ++ z,
              IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V4InV6)
            )
        } |
          (1 to 2).times(`ipv6-seg` ~ ':') ~ ':' ~ (1 to 3).times(`ipv6-seg` ~ ':') ~ `ipv4-adr-bytes` ~> {
            (xs, ys, z) =>

              IpAddress(
                xs.flatMap(_._1) ++ ys.flatMap(_._1) ++ z,
                IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V4InV6)
              )
          } |
          (1 to 3).times(`ipv6-seg` ~ ':') ~ ':' ~ (1 to 2).times(`ipv6-seg` ~ ':') ~ `ipv4-adr-bytes` ~> {
            (xs, ys, z) =>

              IpAddress(
                xs.flatMap(_._1) ++ ys.flatMap(_._1) ++ z,
                IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V4InV6)
              )
          } |
          (1 to 4).times(`ipv6-seg` ~ ':') ~ ':' ~ (1 to 1).times(`ipv6-seg` ~ ':') ~ `ipv4-adr-bytes` ~> {
            (xs, ys, z) =>

              IpAddress(
                xs.flatMap(_._1) ++ ys.flatMap(_._1) ++ z,
                IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V4InV6)
              )
          } |
          (1 to 5).times(`ipv6-seg` ~ ':') ~ ':' ~ `ipv4-adr-bytes` ~> {
            (xs, z) =>

              IpAddress(
                xs.flatMap(_._1) ++ z,
                IpAddress.Representation(xs.length, xs.exists(_._2), Style.V4InV6)
              )
          } |
          (6 to 6).times(`ipv6-seg` ~ ':') ~ `ipv4-adr-bytes` ~> {
            (xs, z) =>

              IpAddress(
                xs.flatMap(_._1) ++ z,
                IpAddress.Representation(-1, xs.exists(_._2), Style.V4InV6)
              )
          } | // (0 to 5).times(...) == ((1 to 5).times(...) | MATCH ~ push(Nil))
          "::" ~ ((1 to 5).times(`ipv6-seg` ~ ':') | MATCH ~ push(Nil)) ~ `ipv4-adr-bytes` ~> {
            (ys, z) =>

              IpAddress(
                ys.flatMap(_._1) ++ z,
                IpAddress.Representation(0, ys.exists(_._2), Style.V4InV6)
              )
          } |
          (1 to 1).times(`ipv6-seg` ~ ':') ~ (1 to 6).times(':' ~ `ipv6-seg`) ~> {
            (xs, ys) =>

              IpAddress(
                xs.flatMap(_._1) ++ ys.flatMap(_._1),
                IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V6)
              )
          } |
          (1 to 2).times(`ipv6-seg` ~ ':') ~ (1 to 5).times(':' ~ `ipv6-seg`) ~> {
            (xs, ys) =>

              IpAddress(
                xs.flatMap(_._1) ++ ys.flatMap(_._1),
                IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V6)
              )
          } |
          (1 to 3).times(`ipv6-seg` ~ ':') ~ (1 to 4).times(':' ~ `ipv6-seg`) ~> {
            (xs, ys) =>

              IpAddress(
                xs.flatMap(_._1) ++ ys.flatMap(_._1),
                IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V6)
              )
          } |
          (1 to 4).times(`ipv6-seg` ~ ':') ~ (1 to 3).times(':' ~ `ipv6-seg`) ~> {
            (xs, ys) =>

              IpAddress(
                xs.flatMap(_._1) ++ ys.flatMap(_._1),
                IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V6)
              )
          } |
          (1 to 5).times(`ipv6-seg` ~ ':') ~ (1 to 2).times(':' ~ `ipv6-seg`) ~> {
            (xs, ys) =>

              IpAddress(
                xs.flatMap(_._1) ++ ys.flatMap(_._1),
                IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V6)
              )
          } |
          (1 to 6).times(`ipv6-seg` ~ ':') ~ (1 to 1).times(':' ~ `ipv6-seg`) ~> {
            (xs, ys) =>

              IpAddress(
                xs.flatMap(_._1) ++ ys.flatMap(_._1),
                IpAddress.Representation(xs.length, (xs ++ ys).exists(_._2), Style.V6)
              )
          } |
          (1 to 7).times(`ipv6-seg` ~ ':') ~ ':' ~> {
            (xs) =>

              IpAddress(
                xs.flatMap(_._1),
                IpAddress.Representation(xs.length, xs.exists(_._2), Style.V6)
              )
          } |
          (7 to 7).times(`ipv6-seg` ~ ':') ~ `ipv6-seg` ~> {
            (xs, x) =>

              IpAddress(
                xs.flatMap(_._1) ++ x._1,
                IpAddress.Representation(-1, (xs :+ x).exists(_._2), Style.V6)
              )
          } |
          ':' ~ (1 to 7).times(':' ~ `ipv6-seg`) ~> {
            (ys) =>

              IpAddress(
                ys.flatMap(_._1),
                IpAddress.Representation(0, ys.exists(_._2), Style.V6)
              )
          } |
          ':' ~ ':' ~ push {

            IpAddress(
              Nil,
              IpAddress.Representation(nullIndex = 0, style = Style.V6)
            )
          }
      }

    /*

    ipv6-literal = '[' ipv6-adr ']'

     */

    def `ipv6-literal`: Rule1[IpAddress] =
      rule {

        '[' ~ `ipv6-adr` ~ ']'

      }

    /*

    ip-adr = ipv6-adr | ipv4-adr

     */

    def `ip-adr`: Rule1[IpAddress] =
      rule {

        `ipv6-adr` | `ipv4-adr`
      }
  }

}
