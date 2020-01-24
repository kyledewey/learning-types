package expressions_and_statements

// definition of custom exception class for ill-typed programs
case class IllTypedException(msg: String) extends Exception(msg)

object Typechecker {
  // Scala's maps are immutable by default.
  // This introduces a type alias; TypeEnv is synonymous with Map[Variable, Type]
  type TypeEnv = Map[Variable, Type]

  def typeof(e: Exp, gamma: TypeEnv): Type = {
    e match {
      case VariableExp(x) if gamma.contains(x) => gamma(x)
      case IntegerExp(_) => IntType
      case TrueExp | FalseExp => BoolType
      case AndExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (BoolType, BoolType) => BoolType
          case _ => throw IllTypedException("and")
        }
      }
      case PlusExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntType, IntType) => IntType
          case _ => throw IllTypedException("plus")
        }
      }
      case LessThanExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntType, IntType) => BoolType
          case _ => throw IllTypedException("less than")
        }
      }
      case _ => throw IllTypedException("other-exp")
    }
  } // typeof

  def typecheckStatement(s: Stmt, gamma: TypeEnv): TypeEnv = {
    s match {
      case LetStmt(x, tau, e) => {
        if (typeof(e, gamma) == tau) {
          gamma + (x -> tau)
        } else {
          throw IllTypedException("let")
        }
      }
      case AssignStmt(x, e) if gamma.contains(x) => {
        val tau = gamma(x)
        if (typeof(e, gamma) == tau) {
          gamma
        } else {
          throw IllTypedException("assign")
        }
      }
      case _ => throw IllTypedException("other-stmt")
    }
  } // typecheckStatement

  def typecheckProgram(input: Program, gamma: TypeEnv): TypeEnv = {
    input match {
      case SingleStatement(s) => typecheckStatement(s, gamma)
      case MultiStatement(s, p) => {
        val gamma2 = typecheckStatement(s, gamma)
        typecheckProgram(p, gamma2)
      }
    }
  } // typecheckProgram
} // Typechecker
