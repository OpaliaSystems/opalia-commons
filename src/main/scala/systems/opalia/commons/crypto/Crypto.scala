package systems.opalia.commons.crypto

import java.security._
import javax.crypto._
import javax.crypto.spec.SecretKeySpec
import scala.util.Try


object Crypto {

  object SignAlgorithm
    extends Enumeration {

    val MD5 = Value("MD5")
    val SHA1 = Value("SHA-1")
    val SHA256 = Value("SHA-256")
  }

  object CipherAlgorithm
    extends Enumeration {

    val AES = Value("AES/ECB/PKCS5Padding")
  }

  def sign(data: String, algorithm: SignAlgorithm.Value, secret: Option[String]): Seq[Byte] =
    sign(data.getBytes, algorithm, secret)

  def sign(data: Seq[Byte], algorithm: SignAlgorithm.Value, secret: Option[String]): Seq[Byte] = {

    val signed =
      secret.map {
        secret =>

          // sign with HMAC support

          val algorithmName = "Hmac" + algorithm.toString.filterNot(_ == '-')
          val handler = Mac.getInstance(algorithmName)

          handler.init(new SecretKeySpec(secret.getBytes, algorithmName))
          handler.doFinal(data.toArray)

      } getOrElse {

        // sign without HMAC support

        val algorithmName = algorithm.toString
        val handler = MessageDigest.getInstance(algorithmName)

        handler.update(data.toArray)
        handler.digest()
      }

    signed
  }

  def encrypt(data: String, secret: String, algorithm: CipherAlgorithm.Value): Seq[Byte] =
    encrypt(data.getBytes, secret, algorithm)

  def encrypt(data: Seq[Byte], secret: String, algorithm: CipherAlgorithm.Value): Seq[Byte] = {

    val handler = Cipher.getInstance(algorithm.toString)
    val key = generateKey(secret, algorithm)

    handler.init(Cipher.ENCRYPT_MODE, key)
    handler.doFinal(data.toArray)
  }

  def decrypt(data: Seq[Byte], secret: String, algorithm: CipherAlgorithm.Value): Seq[Byte] = {

    val handler = Cipher.getInstance(algorithm.toString)
    val key = generateKey(secret, algorithm)

    handler.init(Cipher.DECRYPT_MODE, key)
    Try(handler.doFinal(data.toArray)).getOrElse(Array()).toSeq
  }

  private def generateKey(secret: String, algorithm: CipherAlgorithm.Value): SecretKeySpec = {

    val algorithmName = algorithm.toString.takeWhile(_ != '/')
    val length = Cipher.getMaxAllowedKeyLength(algorithmName) / 8
    // max allowed length from bits to bytes
    val signed = sign(secret, SignAlgorithm.SHA256, None).toArray
    val key = new SecretKeySpec(signed.take(length), algorithmName)

    key
  }
}
