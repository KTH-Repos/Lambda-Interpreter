package simpret.lexer

import simpret.errors._
import scala.util.parsing.combinator._

// https://www.tutorialspoint.com/scala/scala_regular_expressions.htm


object Lexer extends RegexParsers {
  override def skipWhitespace = false

  /* comment and whitespace handling */
  def skip: Parser[Unit] = rep(whiteSpace | comment) ^^^ ()
  def comment: Parser[Unit] = singleComment | multiComment
  def singleComment: Parser[Unit] = "//" ~ rep(not("\n") ~ ".".r) ^^^ ()
  def multiComment: Parser[Unit] = "/*" ~ rep(not("*/") ~ "(?s).".r) ~ "*/" ^^^ ()

  /* identifiers */
  def ident: Parser[TOKID] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r     ^^ { TOKID(_) }
  }

  /* literals */
  def literals: Parser[Token] =
    positioned (boolt | boolf | intlit)
  def boolt       = "true"         ^^ (_ => TOKBOOL(true) )
  def boolf       = "false"        ^^ (_ => TOKBOOL(false) )

  def intlit: Parser[TOKINT] = positioned {
    "(([-]?)[1-9][0-9]*)|0".r      ^^ { str => TOKINT(Integer.parseInt(str)) }
  }

  /* symbols */
  def symbols: Parser[Token] =
    positioned (condif | condthen | condelse | iszero | plus | assign | semicolon | parl | parr | lam | dot | col | tybool | tyint | tyarr)

  def condif      = "if"           ^^ (_ => TOKIF)
  def condthen    = "then"         ^^ (_ => TOKTHEN)
  def condelse    = "else"         ^^ (_ => TOKELSE)

  def iszero      = "iszero"       ^^ (_ => TOKISZERO)

  def plus        = "+"            ^^ (_ => TOKPLUS)

  def assign      = ":="           ^^ (_ => TOKASSIGN)

  def semicolon   = ";"            ^^ (_ => TOKSC)

  def parl        = "("            ^^ (_ => TOKPARL)
  def parr        = ")"            ^^ (_ => TOKPARR)

  def lam         = "\\"           ^^ (_ => TOKLAM)
  def dot         = "."            ^^ (_ => TOKDOT)
  def col         = ":"            ^^ (_ => TOKCOL)

  def tybool      = "BOOL"         ^^ (_ => TOKTYBOOL)
  def tyint       = "INT"          ^^ (_ => TOKTYINT)
  def tyarr       = "->"           ^^ (_ => TOKTYARROW)


  /* token stream */
  def tokens: Parser[List[Token]] = skip ~> rep1(token <~ skip) <~ eof

  def eof: Parser[String] = "\\z".r | failure("unexpected character")
  def token: Parser[Token] =
    positioned(literals | symbols | ident)

  def apply(input: String): Either[LexerError, List[Token]] = {
    parseAll(tokens, input) match {
      case Success(result, _) => Right(result)
      case failure : NoSuccess => Left(LexerError(Location.fromPos(failure.next.pos), failure.msg))
    }
  }
}
