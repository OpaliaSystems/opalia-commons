package systems.opalia.commons.net

import systems.opalia.commons.utility.RegexParsersEx
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

  private object Parser
    extends RegexParsersEx
      with IpAddress.AbstractParser {

    /*

      HOSTNAME-LABEL

     */

    def `HOSTNAME-LABEL`: Parser[String] =
      """(([a-zA-Z0-9]([a-zA-Z0-9]|-)+[a-zA-Z0-9])|[a-zA-Z0-9]{1,2})""".r ^^ {
        x =>

          if (x.length > 63)
            throw new IllegalArgumentException(
              s"The maximum character length of a hostname label is 63 but ${x.length} found.")

          x
      }

    /*

      HOSTNAME-HIGHEST-LABEL

     */

    def `HOSTNAME-HIGHEST-LABEL`: Parser[String] =
      """([a-zA-Z]+)""".r ^^ {
        x =>

          if (x.length > 63)
            throw new IllegalArgumentException(
              s"The maximum character length of a hostname label is 63 but ${x.length} found.")

          x
      }

    /*

      HOSTNAME = (HOSTNAME-LABEL '.')* HOSTNAME-HIGHEST-LABEL

     */

    def `HOSTNAME`: Parser[String] =
      rep(`HOSTNAME-LABEL` <~ ".") ~ `HOSTNAME-HIGHEST-LABEL` ^^ {
        case xs ~ x => xs.map(_ + ".").mkString + x
      } ^^ {
        x =>

          if (x.length > 253)
            throw new IllegalArgumentException(
              s"The maximum character length of a hostname is 253 but ${x.length} found.")

          x
      }

    /*

      HOST = HOSTNAME | IPv4-ADR | IPv6-LITERAL

     */

    def `HOST`: Parser[Either[IpAddress, String]] =
      `HOSTNAME` ^^ (Right(_)) |||
        `IPv4-ADR` ^^ (Left(_)) |||
        `IPv6-LITERAL` ^^ (Left(_))

    /*

      HOST-SIMPLE = HOSTNAME | IPv4-ADR | IPv6-ADR

     */

    def `HOST-ONLY`: Parser[Either[IpAddress, String]] =
      `HOSTNAME` ^^ (Right(_)) |||
        `IPv4-ADR` ^^ (Left(_)) |||
        `IPv6-ADR` ^^ (Left(_))

    /*

      PORT

     */

    def `PORT`: Parser[Int] =
      """(0|([1-9][0-9]*))""".r ^^ (_.toInt) ^^ {
        x =>

          if (x > 65535)
            throw new IllegalArgumentException(
              s"The highest port number is 65535 but $x found.")

          x
      }

    /*

      EXPRESSION = HOST ':' PORT

     */

    def `EXPRESSION`: Parser[EndpointAddress] =
      `HOST` ~ (":" ~> `PORT`) ^^ {
        case address ~ port => EndpointAddress(address, port)
      }


    /*

      end of expressions

     */

    def parseAll(value: String): EndpointAddress = {

      parseAll(`EXPRESSION`, value) match {
        case Success(result, _) => result
        case failure: NoSuccess => throw new IllegalArgumentException(failure.msg)
      }
    }

    def parseHost(value: String): Either[IpAddress, String] = {

      parseAll(`HOST-ONLY`, value) match {
        case Success(result, _) => result
        case failure: NoSuccess => throw new IllegalArgumentException(failure.msg)
      }
    }
  }

  def apply(host: IpAddress, port: Int): EndpointAddress =
    EndpointAddress(Left(host), port)

  def apply(host: String, port: Int): EndpointAddress =
    EndpointAddress(Parser.parseHost(host), port)

  def apply(value: String): EndpointAddress =
    Parser.parseAll(value)
}
