package functions

case class Variable(name: String)
case class FunctionName(name: String)

sealed trait Type
case object IntType extends Type
case object BoolType extends Type
case class FunctionType(paramTypes: Seq[Type], returnType: Type) extends Type

sealed trait Exp
case class VariableExp(x: Variable) extends Exp
case class IntegerExp(i: Int) extends Exp
case object TrueExp extends Exp
case object FalseExp extends Exp
case class AndExp(e1: Exp, e2: Exp) extends Exp
case class PlusExp(e1: Exp, e2: Exp) extends Exp
case class LessThanExp(e1: Exp, e2: Exp) extends Exp
case class FirstOrderCallExp(fn: FunctionName, params: Seq[Exp]) extends Exp
case class HigherOrderDefExp(params: Seq[(Type, Variable)], body: Exp) extends Exp
case class HigherOrderCallExp(function: Exp, params: Seq[Exp]) extends Exp

sealed trait Stmt
case class LetStmt(x: Variable, tau: Type, e: Exp) extends Stmt
case class AssignStmt(x: Variable, e: Exp) extends Stmt

case class Function(
  returnType: Type,
  name: FunctionName,
  params: Seq[(Type, Variable)],
  statements: Seq[Stmt],
  returnExpression: Exp)

case class Program(functions: Seq[Function])

