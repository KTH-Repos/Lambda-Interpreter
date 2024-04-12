package simpret.interpreter

import simpret.parser._
import simpret.errors._


case class EvaluationException(private val message: String, private val x: AST,
  private val cause: Throwable = None.orNull)
    extends Exception(message, cause)


object Interpreter {
  def errFun(msg: String, x: AST) = throw new EvaluationException(msg, x)

  /* function for defining which AST elements are values */
  def isvalue(x: AST) = x match {
    case BoolLit(_) => true
    case IntLit(_) => true
    case LamExp(_, _) => true
    case _ => false
  }

  /* evaluation function for taking one step at a time */
  def step(tree: AST): Option[AST] = tree match{
      /* case Variable(id) => 
        println("here at variable id")
        return Some(Variable(id))
      case BoolLit(b) =>
        println("here at bool b, " + b)
        return Some(BoolLit(b))
      case IntLit(i) =>
        println("here at intlit i, " + i)
        return Some(IntLit(i)) */

      case CondExp(c, e1, e2) if !isvalue(c) =>
        step(c).map(newC => CondExp(newC, e1, e2))
      case CondExp(BoolLit(true), e1, _) =>
        Some(e1) // If the condition is true, evaluate to the first branch
      case CondExp(BoolLit(false), _, e2) =>
        Some(e2) // If the condition is false, evaluate to the second branch
      
      case PlusExp(IntLit(n1), IntLit(n2)) => 
        println("here at base expression for plusexp " + "n1: " + n1 + " n2: " + n2)
        Some(IntLit(n1 + n2))
      case PlusExp(e1, e2) if !isvalue(e1) => 
        println("here at e1 is not evaluable at plusexp, e1: " + e1)
        step(e1).map(newE1 => PlusExp(newE1, e2))
      case PlusExp(e1, e2) if !isvalue(e2) => 
        println("here at e2 is not evaluable at plusexp, e2: " + e2)
        step(e2).map(newE2 => PlusExp(e1, newE2)) 

      case IsZeroExp(e) if !isvalue(e) =>
        println("here at e is not evaluable at iszeroexp, e: " + e)
        step(e).map(newE => IsZeroExp(newE))
      case IsZeroExp(IntLit(i)) => 
        Some(BoolLit(i == 0)) 

      case LamExp(id, e) if !isvalue(e) =>
        println("here at e is not evaluable at lamexp, e: " + e + " id: " + id)
        Some(e)

      case _ => None // No further steps can be taken
  }

  /* evaluation function to iterate the steps of evaluation */
  def eval(x: AST): AST = step(x) match {
    case None => x
    case Some(x1) => eval(x1)
  }

  /* function to apply the interpreter */
  def apply(x: AST): Either[EvaluationError, AST] = {
    try {
      Right(eval(x))
    } catch {
      case EvaluationException (msg, xe, _) =>
        val msg2 = msg + " -> \r\n" + ASTPrinter.convToStr(xe)
        Left(EvaluationError(Location.fromPos(xe.pos), msg2))
    }
  }
}
