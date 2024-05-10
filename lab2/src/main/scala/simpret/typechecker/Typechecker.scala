package simpret.typechecker

import simpret.parser._
import simpret.errors._

object Typechecker {
  // error handling helper functions
  def errUnknownAST(x: AST) = throw new UnknownASTTypecheckerException(x)
  def errExpectedType(ty_str: String, x: AST) = throw new NotExpectedTypeTypecheckerException(ty_str, x)
  def errVarUnbound(x: AST) = throw new VarUnboundTypecheckerException(x)
  def errAppArgument(ty_param: ASTTY, ty_arg: ASTTY, x: AST) = throw new ApplicationArgumentTypecheckerException(ty_param, ty_arg, x)
  def errBranch(ty1: ASTTY, ty2: ASTTY, x: AST) = throw new BranchMismatchTypecheckerException(ty1, ty2, x)
  def errArrowNotSame(ty_param: ASTTY, ty_res: ASTTY, x: AST) = throw new ArrowNotSameTypecheckerException(ty_param, ty_res, x)
  def errCons(eh_ty: ASTTY, et_lty: ASTTY, x: AST) = throw new ConsMismatchTypecheckerException(eh_ty, et_lty, x)
  def errProjTooSmall(x: AST) = throw new ProjectionTooSmallTypecheckerException(x)
  def errProjTooBig(length: Int, x: AST) = throw new ProjectionTooBigTypecheckerException(length, x)
  def errProjNotField(l: String, x: AST) = throw new ProjectionNotAFieldTypecheckerException(l, x)

  // The recursive typechecking relation
  // If the provided AST is typeable, this function should return its type
  // If not, this function should throw an appropriate TypecheckerException
  def check(x: AST, env: Map[String, ASTTY] = Map.empty): ASTTY = x match {
    
    case BoolLit(_) => BoolTy
    case IntLit(_) => IntTy
     
    case PlusExp(e1, e2) => {
      val e1Type = check(e1, env)
      val e2Type = check(e2, env)

      if (e1Type != IntTy) 
        return errExpectedType("int", e1)
      
      if(e2Type != IntTy)
        return errExpectedType("int", e2)

      IntTy
    }
    case CondExp(c, e1, e2) => {
      val cType = check(c, env)

      if (cType != BoolTy)
        return errExpectedType("boolean", c)

      val e1Type = check(e1, env)

      val e2Type = check(e2, env)

      if (e1Type != e2Type)
        return errBranch(e1Type, e2Type, x)

      e1Type
    } 
    case UMinExp(e) => {
      val eType = check(e, env)
      
      if (eType != IntTy)
        return errExpectedType("int", e)

      IntTy
    }
     
    case LtExp(e1, e2) => {
      val e1Type = check(e1, env)
      val e2Type = check(e2, env)

      if (e1Type != IntTy) 
        return errExpectedType("int", e1)
      
      if(e2Type != IntTy)
        return errExpectedType("int", e2)

      BoolTy
    }

    case LetExp(id, e1, e2) => {
      val t1 = check(e1, env)
      val extendedEnv = env + (id -> t1)
      check(e2, extendedEnv)
    }


    case AppExp(e1, e2) => {
      val t1 = check(e1, env)
      val t2 = check(e2, env)
      t1 match {
        case   ArrowTy(fromType, toType) if fromType == t2 =>
          toType
        case ArrowTy(fromType, _) =>
          
          return errExpectedType("int", e2)
        case _ =>
          //måste ändras 
          return errExpectedType("int", e2)
      }
    }

    case LamExp(param, paramType, body) => {
      val extendedEnv = env + (param -> paramType)
      val bodyType = check(body, extendedEnv)
      ArrowTy(paramType, bodyType)
    }

    case FixAppExp(e) => {
      check(e, env) match {
      case ArrowTy(t, returnType) if t == returnType =>
        returnType
      case _ =>
        //måste ändras 
        return errUnknownAST(e)
      }
    }  

    case Variable(id) =>
      // Look up the type of the variable in the environment
      env.getOrElse(id, errVarUnbound(x))

    case _ => errUnknownAST(x)
  }

  /* function to apply the interpreter */
  def apply(x: AST): Either[TypecheckingError, Unit] = {
    try {
      check(x)
      Right(())
    } catch {
      case ex: TypecheckerException =>
        val msg = ex.message
        val x = ex.x
        val msg2 = msg + " -> \r\n" + ASTPrinter.convToStr(x)
        Left(TypecheckingError(Location.fromPos(x.pos), msg2))
    }
  }
}
