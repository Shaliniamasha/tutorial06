object CaesarCipher {

  def encrypt(text: String, shift: Int): String = {
    text.map { char =>
      if (char.isLetter) {
        val base = if (char.isUpper) 'A' else 'a'
        ((char - base + shift) % 26 + base).toChar
      } else {
        char
      }
    }
  }

  def decrypt(textAndShift: (String,Int)): String = {
    val(text,shift) = textAndShift
    encrypt(text, 26 - shift)
  }

  def cipher(text: String, shift: Int, operation: ((String,Int)) => String): String = {
    operation(text,shift)
  }

   def main(args: Array[String]): Unit = {
    val plaintext = "Hello, World!"
    val shift = 3

    val encryptedText = cipher(plaintext, shift, encrypt)
    val decryptedText = cipher(encryptedText, shift, decrypt)

    println(s"Plaintext: $plaintext")
    println(s"Encrypted Text: $encryptedText")
    println(s"Decrypted Text: $decryptedText")
  }
}
