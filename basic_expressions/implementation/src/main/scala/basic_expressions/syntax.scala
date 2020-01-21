package basic_expressions

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
case class LetExp(x: Variable, tau: Type, e1: Exp, e2: Exp) extends Exp
case class AssignExp(x: Variable, e1: Exp, e2: Exp) extends Exp
