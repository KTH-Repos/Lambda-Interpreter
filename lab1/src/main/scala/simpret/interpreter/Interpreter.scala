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

      case CondExp(c, e1, e2) if !isvalue(c) =>
        step(c).map(newC => CondExp(newC, e1, e2))

      case CondExp(BoolLit(true), e1, _) =>
        Some(e1) 

      case CondExp(BoolLit(false), _, e2) =>
        Some(e2) 
      
      case PlusExp(IntLit(n1), IntLit(n2)) => 
        Some(IntLit(n1 + n2))

      case PlusExp(e1, e2) if !isvalue(e1) => 
        step(e1).map(newE1 => PlusExp(newE1, e2))

      case PlusExp(e1, e2) if !isvalue(e2) => 
        step(e2).map(newE2 => PlusExp(e1, newE2)) 

      case IsZeroExp(e) if !isvalue(e) =>
        step(e).map(newE => IsZeroExp(newE))

      case IsZeroExp(IntLit(i)) => 
        Some(BoolLit(i == 0))

      case AppExp(e1, e2) if !isvalue(e1)  =>
        step(e1).map(newE1 => AppExp(newE1, e2))

      case AppExp(e1, e2) if !isvalue(e2)  =>
        step(e2).map(newE2 => AppExp(e1, newE2))

      case AppExp(LamExp(id, body), v2) if isvalue(v2) =>
        Some(substitute(body, id, v2))

      case _ => None 
  }

  /* evaluation function to iterate the steps of evaluation */
  def eval(x: AST): AST = step(x) match {
    case None => x
    case Some(x1) => eval(x1)
  }

  def substitute(expr: AST, id: String, value: AST): AST = expr match {
    case Variable(varId) if varId == id =>    //repalace value instead of variable
      value  

    case Variable(_) =>     //no substitution of value instead of variable
      expr  

    case BoolLit(_) | IntLit(_) =>       //no substitution of value instead of variable
      expr 

    case CondExp(cond, e1, e2) =>
      CondExp(substitute(cond, id, value), substitute(e1, id, value), substitute(e2, id, value))

    case IsZeroExp(e) =>
      IsZeroExp(substitute(e, id, value))

    case PlusExp(e1, e2) =>
      PlusExp(substitute(e1, id, value), substitute(e2, id, value))

    case LamExp(varId, body) if varId == id =>
      LamExp(varId, body)  // Same id means a new scope, do not substitute in the body

    case LamExp(varId, body) =>
      if (freeVars(value).contains(varId)) {
        // Variable `varId` is in the set of free variables of `value`, need renaming
        val newId = newVarId(varId, freeVars(body) ++ freeVars(value))
        val renamedBody = substitute(body, varId, Variable(newId))
        LamExp(newId, substitute(renamedBody, id, value))
      } else {
        LamExp(varId, substitute(body, id, value))
      }

    case AppExp(e1, e2) =>
      AppExp(substitute(e1, id, value), substitute(e2, id, value))
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

  def freeVars(expr: AST): Set[String] = expr match {
    case Variable(id) => 
      Set(id)
    case BoolLit(_) | IntLit(_) => 
      Set.empty
    case CondExp(cond, e1, e2) => 
      freeVars(cond) ++ freeVars(e1) ++ freeVars(e2)
    case IsZeroExp(e) => 
      freeVars(e)
    case PlusExp(e1, e2) => 
      freeVars(e1) ++ freeVars(e2)
    case LamExp(id, body) => 
      freeVars(body)
    case AppExp(e1, e2) => 
      freeVars(e1) ++ freeVars(e2)
  }

  def newVarId(oldId: String, value: Set[String]): String = {
    var suffix = "_"
    var newId = oldId + suffix
    if (value.contains(newId)) {
      newId = newId + suffix
    }
    newId
  }
}
