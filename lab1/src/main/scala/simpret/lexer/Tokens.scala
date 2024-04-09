package simpret.lexer

import scala.util.parsing.input.Positional


sealed trait Token extends Positional


/* ------------- expression literals ---------------- */
case class TOKID(str: String) extends Token
case class TOKBOOL(i: Boolean) extends Token
case class TOKINT(i: Int) extends Token


/* -------------- expression symbols ---------------- */
case object TOKIF extends Token        // "if"
case object TOKTHEN extends Token      // "then"
case object TOKELSE extends Token      // "else"

case object TOKISZERO extends Token    // "iszero"

case object TOKPLUS extends Token      // "+"

case object TOKASSIGN extends Token    // ":="

case object TOKSC extends Token        // ";"

case object TOKPARL extends Token      // "("
case object TOKPARR extends Token      // ")"

case object TOKLAM extends Token       // "\" lambda
case object TOKCOL extends Token       // ":"
case object TOKDOT extends Token       // "."


/* ------------------ type symbols ------------------ */
case object TOKTYBOOL extends Token    // "BOOL"
case object TOKTYINT extends Token     // "INT"
case object TOKTYARROW extends Token   // "->"
