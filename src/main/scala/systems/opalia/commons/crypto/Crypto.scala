package systems.opalia.commons.crypto

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.security._
import javax.crypto.spec._
import javax.crypto.{Cipher => JCipher, _}
import systems.opalia.commons.application.SystemProperty


object Crypto {

  trait CryptoAlgorithm {

    val name: String
    protected val identifier: String
  }

  trait Digester
    extends CryptoAlgorithm {

    def sign(data: String): Seq[Byte] =
      sign(data.getBytes(SystemProperty.defaultCharset))

    def sign(data: Seq[Byte]): Seq[Byte] = {

      val inStream = sign(new ByteArrayInputStream(data.toArray))

      inStream.readAll()
      inStream.getDigester.doFinal()
    }

    def sign(data: InputStream): DigesterInputStream = {

      val handler = initialize()

      new DigesterInputStream(data, handler)
    }

    private def initialize(): DigesterWrapper = {

      val handler = MessageDigest.getInstance(identifier)

      new DigesterWrapper() {

        def update(byte: Byte): Unit =
          handler.update(byte)

        def update(bytes: Array[Byte]): Unit =
          handler.update(bytes)

        def update(bytes: Array[Byte], i: Int, i1: Int): Unit =
          handler.update(bytes, i, i1)

        def doFinal(): Array[Byte] =
          handler.digest()

        override def toString: String =
          handler.toString
      }
    }
  }

  trait DigesterHMAC
    extends CryptoAlgorithm {

    def sign(data: String, secret: String): Seq[Byte] =
      sign(data.getBytes(SystemProperty.defaultCharset), secret)

    def sign(data: Seq[Byte], secret: String): Seq[Byte] = {

      val inStream = sign(new ByteArrayInputStream(data.toArray), secret)

      inStream.readAll()
      inStream.getDigester.doFinal()
    }

    def sign(data: InputStream, secret: String): DigesterInputStream = {

      val handler = initialize(secret)

      new DigesterInputStream(data, handler)
    }

    private def initialize(secret: String): DigesterWrapper = {

      val handler = Mac.getInstance(identifier)

      handler.init(new SecretKeySpec(secret.getBytes(SystemProperty.defaultCharset), identifier))

      new DigesterWrapper() {

        def update(byte: Byte): Unit =
          handler.update(byte)

        def update(bytes: Array[Byte]): Unit =
          handler.update(bytes)

        def update(bytes: Array[Byte], i: Int, i1: Int): Unit =
          handler.update(bytes, i, i1)

        def doFinal(): Array[Byte] =
          handler.doFinal()

        override def toString: String =
          handler.toString
      }
    }
  }

  trait Cipher
    extends CryptoAlgorithm {

    protected val keyLength: Int
    protected val ivLength: Int

    def encrypt(data: String, secret: String): Seq[Byte] =
      encrypt(data.getBytes(SystemProperty.defaultCharset), secret)

    def encrypt(data: Seq[Byte], secret: String): Seq[Byte] = {

      val outStream = new ByteArrayOutputStream()
      val outStreamCypher = encrypt(outStream, secret)

      outStreamCypher.write(data.toArray)
      outStreamCypher.flush()
      outStreamCypher.close()

      outStream.toByteArray
    }

    def encrypt(data: InputStream, secret: String): CipherInputStream = {

      val handler = initialize(enc = true, secret)

      new CipherInputStream(data, handler)
    }

    def encrypt(data: OutputStream, secret: String): CipherOutputStream = {

      val handler = initialize(enc = true, secret)

      new CipherOutputStream(data, handler)
    }

    def decrypt(data: Seq[Byte], secret: String): Seq[Byte] = {

      val outStream = new ByteArrayOutputStream()
      val outStreamCypher = decrypt(outStream, secret)

      outStreamCypher.write(data.toArray)
      outStreamCypher.flush()
      outStreamCypher.close()

      outStream.toByteArray
    }

    def decrypt(data: InputStream, secret: String): CipherInputStream = {

      val handler = initialize(enc = false, secret)

      new CipherInputStream(data, handler)
    }

    def decrypt(data: OutputStream, secret: String): CipherOutputStream = {

      val handler = initialize(enc = false, secret)

      new CipherOutputStream(data, handler)
    }

    private def initialize(enc: Boolean, secret: String): JCipher = {

      val handler = JCipher.getInstance(identifier)

      val signed = Digester.SHA256.sign(secret).toArray
      val key = new SecretKeySpec(signed.take(keyLength), name)
      val iv = new IvParameterSpec(signed.drop(keyLength).take(ivLength))

      if (enc)
        handler.init(JCipher.ENCRYPT_MODE, key, iv)
      else
        handler.init(JCipher.DECRYPT_MODE, key, iv)

      handler
    }
  }

  object Digester {

    object MD5
      extends Digester {

      val name = "MD5"
      val identifier = "MD5"
    }

    object SHA1
      extends Digester {

      val name = "SHA-1"
      val identifier = "SHA-1"
    }

    object SHA256
      extends Digester {

      val name = "SHA-256"
      val identifier = "SHA-256"
    }

    object SHA384
      extends Digester {

      val name = "SHA-384"
      val identifier = "SHA-384"
    }

    object SHA512
      extends Digester {

      val name = "SHA-512"
      val identifier = "SHA-512"
    }

  }

  object DigesterHMAC {

    object MD5
      extends DigesterHMAC {

      val name = "MD5"
      val identifier = "HmacMD5"
    }

    object SHA1
      extends DigesterHMAC {

      val name = "SHA-1"
      val identifier = "HmacSHA1"
    }

    object SHA256
      extends DigesterHMAC {

      val name = "SHA-256"
      val identifier = "HmacSHA256"
    }

    object SHA384
      extends DigesterHMAC {

      val name = "SHA-384"
      val identifier = "HmacSHA384"
    }

    object SHA512
      extends DigesterHMAC {

      val name = "SHA-512"
      val identifier = "HmacSHA512"
    }

  }

  object Cipher {

    object AES
      extends Cipher {

      val name = "AES"
      val identifier = "AES/CBC/PKCS5Padding"
      val keyLength = 16
      val ivLength = 16
    }

    object DES
      extends Cipher {

      val name = "DES"
      val identifier = "DES/CBC/PKCS5Padding"
      val keyLength = 8
      val ivLength = 8
    }

    object DESede
      extends Cipher {

      val name = "DESede"
      val identifier = "DESede/CBC/PKCS5Padding"
      val keyLength = 24
      val ivLength = 8
    }

  }

}
