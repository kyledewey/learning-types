package functions

// definition of custom exception class for ill-typed programs
case class IllTypedException(msg: String) extends Exception(msg)

object Typechecker {
  type SymbolTable = Map[FunctionName, (Type, Seq[Type])]

  def makeSymbolTable(p: Program): SymbolTable = {
    p.functions.foldLeft(Map(): SymbolTable)((res, cur) => {
      val Function(returnType, name, params, _, _) = cur
      if (res.contains(name)) {
        throw IllTypedException("duplicate function name: " + name.name)
      }
      val paramNames = params.map(_._2).toSet
      if (paramNames.size != params.size) {
        throw IllTypedException("duplicate parameter name")
      }
      res + (name -> (returnType -> params.map(_._1)))
    })
  }

  def allDistinct[A](items: Seq[A]): Boolean = {
    items.toSet.size == items.size
  }

  // also typechecks the input program
  def apply(p: Program): Typechecker = {
    val retval = new Typechecker(makeSymbolTable(p))
    retval.typecheckProgram(p)
    retval
  }
} // Typechecker
import Typechecker.SymbolTable

// The typechecker itself is associated with a symbol table, which acts as
// a separate parameter
class Typechecker(val st: SymbolTable) {
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
      case FirstOrderCallExp(fn, e) if st.contains(fn) => {
        val (tau1, tau2) = st(fn)
        if (e.size != tau2.size) {
          throw IllTypedException("wrong number of params")
        } else {
          if (e.map(curE => typeof(curE, gamma)) != tau2) {
            throw IllTypedException("parameter type mismatch")
          } else {
            tau1
          }
        }
      }
      case HigherOrderDefExp(params, e) if Typechecker.allDistinct(params.map(_._2)) => {
        val gamma2 = gamma ++ params.map(pair => (pair._2 -> pair._1))
        val tau2 = typeof(e, gamma2)
        FunctionType(params.map(_._1), tau2)
      }
      case HigherOrderCallExp(e1, e2) => {
        typeof(e1, gamma) match {
          case FunctionType(tau1, tau2) if tau1.size == e2.size => {
            if (e2.map(e => typeof(e, gamma)) == tau1) {
              tau2
            } else {
              throw IllTypedException("parameter type mismatch")
            }
          }
          case _ => throw IllTypedException("not a higher-order function")
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

  def typecheckFunction(f: Function) {
    val gamma1 = f.params.map(pair => (pair._2 -> pair._1)).toMap
    val gamma2 = f.statements.foldLeft(gamma1)((res, cur) =>
      typecheckStatement(cur, res))
    if (typeof(f.returnExpression, gamma2) != f.returnType) {
      throw IllTypedException("return type mismatch")
    }
  }

  def typecheckProgram(input: Program) {
    input.functions.foreach(typecheckFunction)
  }
} // Typechecker
