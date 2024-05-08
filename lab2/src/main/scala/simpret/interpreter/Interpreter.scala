package simpret.interpreter

import simpret.parser._
import simpret.errors._


abstract case class EvaluationException(val message: String, val x: AST,
                                     private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

final class VariableCapturedEvaluationException(val var_id: String, val subst_s: AST,
                                                private val cause: Throwable = None.orNull)
  extends EvaluationException("variable (" + var_id + ") has been captured during substitution", subst_s, cause)

object Interpreter {
  def errVarCap(var_id: String, x: AST) = throw new VariableCapturedEvaluationException(var_id, x)

  /* function for defining which AST elements are values */
  def isvalue(x: AST): Boolean = x match {
    case BoolLit(_) => true
    case IntLit(_) => true
    case LamExp(_,_,_) => true
    case NilExp(_) => true
    case ConsExp(eh, et) => isvalue(eh) & isvalue(et)
    case TupleExp(el) => el.forall(isvalue)
    case RecordExp(em) => em.values.forall(isvalue)
    case _ => false
  }

  /* function for determining the free variables of an expression */
  //def freevars (x: AST) : List[String] = throw new Exception("implement me")

  /* function for carrying out a substitution */
  //def subst (x: String, s: AST, t: AST):AST = throw new Exception("implement me")
  def substitute(expr: AST, id: String, value: AST): AST = expr match {
    case Variable(varId) if varId == id =>    //repalace value instead of variable
      value  

    case Variable(_) =>     //no substitution of value instead of variable
      expr  

    case BoolLit(_) | IntLit(_) =>       //no substitution of value instead of variable
      expr 

    case CondExp(cond, e1, e2) =>
      CondExp(substitute(cond, id, value), substitute(e1, id, value), substitute(e2, id, value))


    case PlusExp(e1, e2) =>
      PlusExp(substitute(e1, id, value), substitute(e2, id, value))
    /*
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
      */

    case AppExp(e1, e2) =>
      AppExp(substitute(e1, id, value), substitute(e2, id, value))
  }
  /* Evaluation function for taking one reduction step at a time.
     This function should return the stepped AST if a reduction step can be taken.
     If the provided AST cannot be reduced further, this function should
     return None.*/
  def step(tree: AST): Option[AST] = tree match {
    

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

      case AppExp(e1, e2) if !isvalue(e1)  =>
        step(e1).map(newE1 => AppExp(newE1, e2))

      case AppExp(e1, e2) if !isvalue(e2)  =>
        step(e2).map(newE2 => AppExp(e1, newE2))

      /*
      case AppExp(LamExp(id, body), v2) if isvalue(v2) =>
        Some(substitute(body, id, v2))
        */

      
      case _ => None 
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
  
  def freeVars(expr: AST): Set[String] = expr match {
    case Variable(id) => 
      Set(id)
    case BoolLit(_) | IntLit(_) => 
      Set.empty
    case CondExp(cond, e1, e2) => 
      freeVars(cond) ++ freeVars(e1) ++ freeVars(e2)
   
    case PlusExp(e1, e2) => 
      freeVars(e1) ++ freeVars(e2)
    /* case LamExp(id, body) => 
      freeVars(body)
      */
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
