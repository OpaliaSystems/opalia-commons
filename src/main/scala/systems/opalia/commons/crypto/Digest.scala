package systems.opalia.commons.crypto

import java.io.{ByteArrayInputStream, InputStream}
import java.security.{MessageDigest, Security}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import org.bouncycastle.jce.provider.BouncyCastleProvider
import systems.opalia.commons.codec.Hex
import systems.opalia.interfaces.rendering.Renderer


trait Digest {

  val algorithm: Digest.Algorithm.Value
  val hmac: Boolean

  def sign(data: String): IndexedSeq[Byte]

  def sign(data: String, charset: String): IndexedSeq[Byte]

  def sign(data: IndexedSeq[Byte]): IndexedSeq[Byte]

  def sign(data: InputStream): DigestInputStream

  def signToOctetString(data: String): String

  def signToOctetString(data: String, charset: String): String

  def signToOctetString(data: IndexedSeq[Byte]): String
}

object Digest {

  def apply(algorithm: Algorithm.Value): Digest = {

    Security.addProvider(new BouncyCastleProvider())

    create(algorithm, None)
  }

  def apply(algorithm: Algorithm.Value, secret: String): Digest =
    apply(algorithm, secret, Renderer.defaultCharset)

  def apply(algorithm: Algorithm.Value, secret: String, charset: String): Digest =
    apply(algorithm, secret.getBytes(charset))

  def apply(algorithm: Algorithm.Value, secret: IndexedSeq[Byte]): Digest = {

    if (secret.isEmpty)
      throw new IllegalArgumentException("Expect non empty secret.")

    Security.addProvider(new BouncyCastleProvider())

    create(algorithm, Some(secret))
  }

  private def create(algorithmChoice: Algorithm.Value, secret: Option[IndexedSeq[Byte]]): Digest = {

    new Digest {

      val algorithm: Algorithm.Value = algorithmChoice
      val hmac: Boolean = secret.isDefined

      def sign(data: String): IndexedSeq[Byte] =
        sign(data, Renderer.defaultCharset)

      def sign(data: String, charset: String): IndexedSeq[Byte] =
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

      def signToOctetString(data: String, charset: String): String =
        Hex.encode(sign(data, charset))

      def signToOctetString(data: IndexedSeq[Byte]): String =
        Hex.encode(sign(data))
    }
  }

  object Algorithm
    extends Enumeration {

    val MD5 = Value("MD5")
    val SHA1 = Value("SHA1")
    val SHA256 = Value("SHA256")
    val SHA384 = Value("SHA384")
    val SHA512 = Value("SHA512")

    def withNameOpt(string: String): Option[Value] =
      values.find(_.toString == string.toUpperCase.replace("SHA-", "SHA"))
  }

}
