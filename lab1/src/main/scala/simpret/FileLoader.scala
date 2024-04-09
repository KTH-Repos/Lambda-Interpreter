package simpret

import simpret.errors.InputError

import scala.io.Source



object FileLoader {
  val debugging = false

  /* function to apply the file loader */
  def apply(filename: String): Either[InputError, String] = {
    try {
      if (!debugging) {
        val fileContents = Source.fromFile(filename).mkString
        Right(fileContents)
      } else {
        val input = "x := 1; if iszero x then (y:=3;z:=y) else (z:=2);z"
        Right(input)
      }
    } catch {
      case ex : Exception => Left(InputError(ex.getMessage()))
    }
  }
}
