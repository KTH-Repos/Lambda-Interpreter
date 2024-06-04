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
    case Variable(varId) if varId == id =>
      value

    case Variable(_) =>
      expr

    case BoolLit(_) | IntLit(_) =>
      expr

    case CondExp(cond, e1, e2) =>
      CondExp(substitute(cond, id, value), substitute(e1, id, value), substitute(e2, id, value))

    case PlusExp(e1, e2) =>
      PlusExp(substitute(e1, id, value), substitute(e2, id, value))

    case LamExp(varId, ty, body) if varId == id =>
      LamExp(varId, ty, body)

    case LamExp(varId, ty, body) =>
      if (freeVars(value).contains(varId)) {
        val newId = newVarId(varId, freeVars(body) ++ freeVars(value))
        val renamedBody = substitute(body, varId, Variable(newId))
        LamExp(newId, ty, substitute(renamedBody, id, value))
      } else {
        LamExp(varId, ty, substitute(body, id, value))
      }

    case LetExp(varId, e1, e2) =>
      if (id == varId || freeVars(value).contains(varId)) {
        LetExp(varId, substitute(e1, id, value), e2)
      } else {
        LetExp(varId, substitute(e1, id, value), substitute(e2, id, value))
      }

    case AppExp(e1, e2) =>
      AppExp(substitute(e1, id, value), substitute(e2, id, value))

    case LtExp(e1, e2) =>
      LtExp(substitute(e1, id, value), substitute(e2, id, value))

    case UMinExp(e) =>
      UMinExp(substitute(e, id, value))

    case FixAppExp(LamExp(varId, ty, body)) =>
      // Substitute the body of the lambda with the FixAppExp itself.
      FixAppExp(LamExp(varId, ty, substitute(body, id, value)))

    case FixAppExp(f) =>
      FixAppExp(substitute(f, id, value))

    case ConsExp(eh, et) =>
      ConsExp(substitute(eh, id, value), substitute(et, id, value))

    case NilExp(_) =>
      expr

    case TupleExp(elements) =>
      TupleExp(elements.map(e => substitute(e, id, value)))

    case RecordExp(map) =>
      RecordExp(map.map { case (label, expr) => (label, substitute(expr, id, value)) })

    case TailExp(e) =>
      TailExp(substitute(e, id, value))

    case HeadExp(e) =>
      HeadExp(substitute(e, id, value))

    case IsNilExp(e) =>
      IsNilExp(substitute(e, id, value))

    case ProjTupleExp(e, i) =>
      ProjTupleExp(substitute(e, id, value), i)

    case ProjRecordExp(e, l) =>
      ProjRecordExp(substitute(e, id, value), l)
  }

  /* Evaluation function for taking one reduction step at a time.
     This function should return the stepped AST if a reduction step can be taken.
     If the provided AST cannot be reduced further, this function should
     return None.*/
  def step(tree: AST): Option[AST] = tree match {
      case UMinExp(IntLit(n))  =>
        Some(IntLit(-n))

      case UMinExp(e1) if !isvalue(e1) =>
        step(e1).map(newE1 => UMinExp(newE1))

      case CondExp(BoolLit(true), e1, _) =>
        Some(e1) 

      case CondExp(BoolLit(false), _, e2) =>
        Some(e2)

      case CondExp(cond, e1, e2) if !isvalue(cond) =>
        step(cond).map(newCond => CondExp(newCond, e1, e2))

      case LtExp(IntLit(n1), IntLit(n2)) => 
        Some(BoolLit(n1 < n2))

      case LtExp(e1, e2) if !isvalue(e1) =>
        step(e1).map(newE1 => LtExp(newE1, e2)) 

      case LtExp(e1, e2) if !isvalue(e2) =>
        step(e2).map(newE2 => LtExp(e1, newE2))

      case LetExp(x, e1, t2) if isvalue(e1) =>
        // This case handles when `v1` is a value. We substitute `x` with `v1` in `t2`.
        Some(substitute(t2, x, e1))

      case LetExp(x, e1, t2) if !isvalue(e1) =>
        // This case handles when `e1` is not a value and needs to be evaluated.
        step(e1).map(newE1 => LetExp(x, newE1, t2))

      case FixAppExp(LamExp(funcId, ArrowTy(argIn, argOut), iteratorFunc @ LamExp(iterId, iterTy, body))) =>
        // Substitute the recursive function identifier with the fixed-point expression in the body of the iterator function
        val substitutedBody = substitute(body, funcId, FixAppExp(LamExp(funcId, ArrowTy(argIn, argOut), iteratorFunc)))
        // Create a new lambda expression with the substituted body
        val newIteratorFunc = LamExp(iterId, iterTy, substitutedBody)
        // Return the new iterator function
        Some(newIteratorFunc)

      case FixAppExp(e) if !isvalue(e) =>
        // If the expression inside FixAppExp is not a value, continue evaluation
        step(e).map(FixAppExp(_))

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
  
      case AppExp(LamExp(id, ty, body), v2) if isvalue(v2) =>
        Some(substitute(body, id, v2))

      case TupleExp(elements) =>
        val evaluatedElements = elements.takeWhile(isvalue(_)) 
        if (evaluatedElements.size < elements.size) {
          step(elements(evaluatedElements.size)) match {
            case Some(a) =>
              Some(TupleExp(elements.patch(evaluatedElements.size, List(a), 1)))
            case None =>
              None
          }
        } else {
          None
        }

      case ProjTupleExp(TupleExp(elements), i) =>
        val evaluatedElements = elements.map {
          case e if isvalue(e) => e
          case e => step(e) match {
            case Some(evaluated) => evaluated
            case None => e // If step returns None, use the original expression
          }
        }
        if (i >= 1 && i <= evaluatedElements.length) {
          Some(evaluatedElements(i - 1))
        } else {
          None // Index out of bounds. Evaluator shall give Evaluation stuck!
        }

      case ProjTupleExp(e, i) if !isvalue(e) =>
        step(e).map(newE => ProjTupleExp(newE, i))

      case RecordExp(map) =>
        val newMap = map.flatMap {
          case (label, expr) =>
            if (!isvalue(expr)) {
              step(expr).map(newExpr => (label, newExpr))
            } else {
              Some((label, expr))
            }
        }

        if (newMap != map) {
          Some(RecordExp(newMap))
        } else {
          None
        }
      
      case ProjRecordExp(RecordExp(map), l) =>
        map.get(l) match {
          case Some(expr) => Some(expr)
          case None => None
        }

      case ProjRecordExp(e, l) if !isvalue(e) =>
        step(e).map(newE => ProjRecordExp(newE, l))

      case ConsExp(IntLit(n), TupleExp(lst)) =>
        Some(TupleExp(IntLit(n) :: lst))

      case ConsExp(eh, TupleExp(lst)) if isvalue(eh) =>
        Some(TupleExp(eh :: lst))

      case ConsExp(TupleExp(lst1), TupleExp(lst2)) =>
        Some(TupleExp(lst1 ++ lst2))

      case ConsExp(eh, et) if !isvalue(eh) =>
        step(eh).map(newEh => ConsExp(newEh, et))

      case ConsExp(eh, et) if !isvalue(et) =>
        step(et).map(newEt => ConsExp(eh, newEt))

      case IsNilExp(NilExp(_)) =>
        Some(BoolLit(true))

      case IsNilExp(ConsExp(eh, et)) if isvalue(ConsExp(eh, et)) =>
        Some(BoolLit(false))

      case IsNilExp(e) if !isvalue(e) =>
        step(e).map(newE => IsNilExp(newE))

      case HeadExp(NilExp(ty)) =>
        Some(NilExp(ty))
      
      case HeadExp(ConsExp(eh, et)) if isvalue(ConsExp(eh, et)) =>
        Some(eh)

      case HeadExp(e) if !isvalue(e) =>
        step(e).map(newE => HeadExp(newE))

      case TailExp(NilExp(ty)) =>
        Some(NilExp(ty))

      case TailExp(ConsExp(eh, et)) if isvalue(ConsExp(eh, et)) =>
        Some(et)
      
      case TailExp(e) if !isvalue(e) =>
        step(e).map(newE => TailExp(newE))

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

    case LamExp(id, ty, body) => 
      freeVars(body)

    case FixAppExp(e) =>
    freeVars(e)   

    case AppExp(e1, e2) => 
      freeVars(e1) ++ freeVars(e2)

    case TupleExp(elements) =>
      elements.flatMap(freeVars).toSet

    case RecordExp(map) =>
      map.values.flatMap(freeVars).toSet

    case ProjTupleExp(e, _) =>
      freeVars(e)

    case ProjRecordExp(e, _) => 
    freeVars(e)

    case LtExp(e1, e2) =>
      freeVars(e1) ++ freeVars(e2)

    case UMinExp(e) =>
      freeVars(e)

    case ConsExp(eh, et) =>
      freeVars(eh) ++ freeVars(et)

    case NilExp(_) =>
      Set.empty

    case IsNilExp(e) =>
      freeVars(e)

    case HeadExp(e) =>
      freeVars(e)

    case TailExp(e) =>
      freeVars(e)

    case LetExp(x, e1, e2) => 
      freeVars(e1) ++ (freeVars(e2) - x)
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
