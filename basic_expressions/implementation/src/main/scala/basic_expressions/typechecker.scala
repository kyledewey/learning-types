package basic_expressions

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
      case LetExp(x, tau1, e1, e2) => {
        if (typeof(e1, gamma) == tau1) {
          typeof(e2, gamma + (x -> tau1))
        } else {
          throw IllTypedException("let")
        }
      }
      case AssignExp(x, e1, e2) if gamma.contains(x) => {
        val tau1 = gamma(x)
        if (typeof(e1, gamma) == tau1) {
          typeof(e2, gamma)
        } else {
          throw IllTypedException("assign")
        }
      }
      case _ => throw IllTypedException("other")
    }
  } // typeof
} // Typechecker
