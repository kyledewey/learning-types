package expressions_and_statements

case class Variable(name: String)

sealed trait Type
case object IntType extends Type
case object BoolType extends Type

sealed trait Exp
case class VariableExp(x: Variable) extends Exp
case class IntegerExp(i: Int) extends Exp
case object TrueExp extends Exp
case object FalseExp extends Exp
case class AndExp(e1: Exp, e2: Exp) extends Exp
case class PlusExp(e1: Exp, e2: Exp) extends Exp
case class LessThanExp(e1: Exp, e2: Exp) extends Exp

sealed trait Stmt
case class LetStmt(x: Variable, tau: Type, e: Exp) extends Stmt
case class AssignStmt(x: Variable, e: Exp) extends Stmt

sealed trait Program
case class SingleStatement(s: Stmt) extends Program
case class MultiStatement(s: Stmt, p: Program) extends Program

