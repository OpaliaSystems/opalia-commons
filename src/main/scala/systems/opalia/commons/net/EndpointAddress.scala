package systems.opalia.commons.net

import org.parboiled2._
import scala.util.{Failure, Success}
import systems.opalia.interfaces.rendering._


case class EndpointAddress private(host: Either[IpAddress, String], port: Int)
  extends StringRenderable {

  def renderString(renderer: StringRenderer): StringRenderer = {

    host match {
      case Left(x) if (x.representation.style == IpAddress.Style.V4) => renderer ~ x ~ ':' ~ port
      case Left(x) => renderer ~ '[' ~ x ~ "]:" ~ port
      case Right(x) => renderer ~ x ~ ':' ~ port
    }
  }

  def hostString: String = {

    host match {
      case Left(x) => x.toString
      case Right(x) => x
    }
  }
}

object EndpointAddress {

  val maxPort: Int = 65535

  private class EndpointAddressParser(val input: ParserInput)
    extends Parser
      with IpAddress.AbstractParser {

    private val LabelChars = CharPredicate.AlphaNum ++ '-'

    /*

      hostname-label

     */

    def `hostname-label`: Rule1[String] =
      rule {

        capture(oneOrMore(LabelChars)) ~>
          ((x: String) => test(x.length <= 63 && x.head != '-' && x.last != '-') ~ push(x))
      }

    /*

      hostname = (hostname-label '.')* hostname-label

     */

    def `hostname`: Rule1[String] =
      rule {

        oneOrMore(`hostname-label`).separatedBy('.') ~> (_.mkString(".")) ~>
          ((x: String) => test(x.length <= 253) ~ push(x))
      }

    /*

      host = ipv6-literal | ipv4-adr | hostname

     */

    def `host`: Rule1[Either[IpAddress, String]] =
      rule {

        (`ipv6-literal` ~ &(':') ~> ((x: IpAddress) => Left(x))) |
          (`ipv4-adr` ~ &(':') ~> ((x: IpAddress) => Left(x))) |
          (`hostname` ~ &(':') ~> ((x: String) => Right(x)))
      }

    /*

      host-only = ipv6-adr | ipv4-adr | hostname

     */

    def `host-expression`: Rule1[Either[IpAddress, String]] =
      rule {

        (`ipv6-adr` ~ EOI ~> ((x: IpAddress) => Left(x))) |
          (`ipv4-adr` ~ EOI ~> ((x: IpAddress) => Left(x))) |
          (`hostname` ~ EOI ~> ((x: String) => Right(x)))
      }

    /*

      port

     */

    def `port`: Rule1[Int] =
      rule {

        capture((CharPredicate.Digit19 ~ zeroOrMore(CharPredicate.Digit)) | '0') ~> ((x: String) => BigInt(x)) ~>
          ((x: BigInt) => test(x <= maxPort) ~ push(x.toInt))
      }

    /*

      expression = host ':' port

     */

    def `endpoint-expression`: Rule1[EndpointAddress] =
      rule {

        `host` ~ ':' ~ `port` ~ EOI ~> {
          (address: Either[IpAddress, String], port: Int) =>

            EndpointAddress(address, port)
        }
      }
  }

  def apply(host: IpAddress, port: Int): EndpointAddress = {

    if (port < 0 || port > EndpointAddress.maxPort)
      throw new IllegalArgumentException("Invalid value for endpoint port.")

    EndpointAddress(Left(host), port)
  }

  def apply(host: String, port: Int): EndpointAddress = {

    val parser = new EndpointAddressParser(host)

    val result = parser.`host-expression`.run() match {
      case Failure(e: ParseError) =>
        throw new IllegalArgumentException(s"Failed to parse endpoint host.\n${parser.formatError(e)}")
      case Failure(e) =>
        throw e
      case Success(x) =>
        x
    }

    if (port < 0 || port > EndpointAddress.maxPort)
      throw new IllegalArgumentException("Invalid value for endpoint port.")

    EndpointAddress(result, port)
  }

  def apply(address: String): EndpointAddress = {

    val parser = new EndpointAddressParser(address)

    parser.`endpoint-expression`.run() match {
      case Failure(e: ParseError) =>
        throw new IllegalArgumentException(s"Failed to parse endpoint address.\n${parser.formatError(e)}")
      case Failure(e) =>
        throw e
      case Success(x) =>
        x
    }
  }
}
