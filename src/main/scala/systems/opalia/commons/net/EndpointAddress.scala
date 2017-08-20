package systems.opalia.commons.net

import java.util.Objects
import systems.opalia.commons.utility.RegexParsersEx


class EndpointAddress private(val host: String,
                              val port: Int,
                              val hostType: HostType.Value) {

  override def equals(that: Any): Boolean =
    that match {

      case that: EndpointAddress if (
        this.host == that.host &&
          this.port == that.port &&
          this.hostType == that.hostType) => true
      case _ => false
    }

  override def toString: String =
    if (hostType == HostType.IPv6)
      "[" + host + "]:" + port.toString
    else
      host + ":" + port.toString

  override def hashCode: Int =
    Objects.hash(host, Int.box(port))
}

object EndpointAddress {

  private object Parser
    extends RegexParsersEx
      with IpAndPortParser {

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

      HOST = HOSTNAME | IPV4ADR | IPV6ADR

     */

    def `HOST`: Parser[(String, HostType.Value)] =
      `HOSTNAME` ^^ ((_, HostType.Hostname)) |
        `IPv4-ADR` ^^ ((_, HostType.IPv4)) |
        `IPv6-LITERAL` ^^ ((_, HostType.IPv6))

    /*

      EXPRESSION = HOST ':' PORT

     */

    def `EXPRESSION`: Parser[EndpointAddress] =
      `HOST` ~ (":" ~> `PORT`) ^^ {
        case address ~ port => new EndpointAddress(address._1.toLowerCase, port, address._2)
      }

    /*

      end of expressions

     */

    def apply(value: String): EndpointAddress = {

      parseAll(`EXPRESSION`, value) match {
        case Success(result, _) => result
        case failure: NoSuccess => throw new IllegalArgumentException(failure.msg)
      }
    }
  }

  def parse(value: String): EndpointAddress =
    Parser(value)
}
