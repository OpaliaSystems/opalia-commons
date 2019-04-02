package systems.opalia.commons.crypto

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.Charset
import java.security.{MessageDigest, Security}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import org.bouncycastle.jce.provider.BouncyCastleProvider
import systems.opalia.commons.codec.Hex
import systems.opalia.interfaces.rendering.Renderer


trait Digest {

  val algorithm: Digest.Algorithm
  val hmac: Boolean

  def sign(data: String): IndexedSeq[Byte]

  def sign(data: String, charset: Charset): IndexedSeq[Byte]

  def sign(data: IndexedSeq[Byte]): IndexedSeq[Byte]

  def sign(data: InputStream): DigestInputStream

  def signToOctetString(data: String): String

  def signToOctetString(data: String, charset: Charset): String

  def signToOctetString(data: IndexedSeq[Byte]): String
}

object Digest {

  def apply(algorithm: Algorithm): Digest = {

    Security.addProvider(new BouncyCastleProvider())

    create(algorithm, None)
  }

  def apply(algorithm: Algorithm, secret: String): Digest =
    apply(algorithm, secret, Renderer.appDefaultCharset)

  def apply(algorithm: Algorithm, secret: String, charset: Charset): Digest =
    apply(algorithm, secret.getBytes(charset))

  def apply(algorithm: Algorithm, secret: IndexedSeq[Byte]): Digest = {

    if (secret.isEmpty)
      throw new IllegalArgumentException("Expect non empty secret.")

    Security.addProvider(new BouncyCastleProvider())

    create(algorithm, Some(secret))
  }

  private def create(algorithmChoice: Algorithm, secret: Option[IndexedSeq[Byte]]): Digest = {

    new Digest {

      val algorithm: Algorithm = algorithmChoice
      val hmac: Boolean = secret.isDefined

      def sign(data: String): IndexedSeq[Byte] =
        sign(data, Renderer.appDefaultCharset)

      def sign(data: String, charset: Charset): IndexedSeq[Byte] =
        sign(data.getBytes(charset))

      def sign(data: IndexedSeq[Byte]): IndexedSeq[Byte] = {

        val inputStream = sign(new ByteArrayInputStream(data.toArray))

        inputStream.readAll()
        inputStream.doFinal()
      }

      def sign(data: InputStream): DigestInputStream = {

        secret match {
          case None => {

            val algorithmName =
              algorithmChoice match {
                case Algorithm.MD5 => "MD5"
                case Algorithm.SHA1 => "SHA-1"
                case Algorithm.SHA256 => "SHA-256"
                case Algorithm.SHA384 => "SHA-384"
                case Algorithm.SHA512 => "SHA-512"
              }

            val handler = MessageDigest.getInstance(algorithmName, "BC")

            new DigestInputStream(data) {

              def update(byte: Byte): Unit =
                handler.update(byte)

              def update(bytes: Array[Byte]): Unit =
                handler.update(bytes)

              def update(bytes: Array[Byte], offset: Int, length: Int): Unit =
                handler.update(bytes, offset, length)

              def doFinal(): Array[Byte] =
                handler.digest()
            }
          }

          case Some(secret) => {

            val algorithmName =
              algorithm match {
                case Algorithm.MD5 => "HmacMD5"
                case Algorithm.SHA1 => "HmacSHA1"
                case Algorithm.SHA256 => "HmacSHA256"
                case Algorithm.SHA384 => "HmacSHA384"
                case Algorithm.SHA512 => "HmacSHA512"
              }

            val handler = Mac.getInstance(algorithmName, "BC")

            handler.init(new SecretKeySpec(secret.toArray, algorithmName))

            new DigestInputStream(data) {

              def update(byte: Byte): Unit =
                handler.update(byte)

              def update(bytes: Array[Byte]): Unit =
                handler.update(bytes)

              def update(bytes: Array[Byte], offset: Int, length: Int): Unit =
                handler.update(bytes, offset, length)

              def doFinal(): Array[Byte] =
                handler.doFinal()
            }
          }
        }
      }

      def signToOctetString(data: String): String =
        Hex.encode(sign(data))

      def signToOctetString(data: String, charset: Charset): String =
        Hex.encode(sign(data, charset))

      def signToOctetString(data: IndexedSeq[Byte]): String =
        Hex.encode(sign(data))
    }
  }

  sealed trait Algorithm

  object Algorithm {

    case object MD5
      extends Algorithm

    case object SHA1
      extends Algorithm

    case object SHA256
      extends Algorithm

    case object SHA384
      extends Algorithm

    case object SHA512
      extends Algorithm

    val values: Seq[Algorithm] =
      MD5 :: SHA1 :: SHA256 :: SHA384 :: SHA512 :: Nil

    def withNameOpt(string: String): Option[Algorithm] =
      values.find(_.toString == string.toUpperCase.replace("SHA-", "SHA"))

    def withName(string: String): Algorithm =
      withNameOpt(string).getOrElse(throw new IllegalArgumentException(s"Cannot find algorithm with name “$string”."))
  }

}
