package functions

import org.scalatest.FlatSpec

class TestTypechecker extends FlatSpec {
  val empty = Typechecker(Program(Seq()))
  val x = Variable("x")
  val y = Variable("y")

  "The typechecker" should "handle variables in scope" in {
    assertResult(IntType) {
      empty.typeof(VariableExp(x), Map(x -> IntType))
    }
  }

  it should "handle variables not in scope" in {
    assertThrows[IllTypedException] {
      empty.typeof(VariableExp(x), Map())
    }
  }

  it should "handle integers" in {
    assertResult(IntType) {
      empty.typeof(IntegerExp(42), Map())
    }
  }

  it should "handle true" in {
    assertResult(BoolType) {
      empty.typeof(TrueExp, Map())
    }
  }

  it should "handle false" in {
    assertResult(BoolType) {
      empty.typeof(FalseExp, Map())
    }
  }

  it should "handle and - well-typed" in {
    assertResult(BoolType) {
      empty.typeof(AndExp(TrueExp, TrueExp), Map())
    }
  }

  it should "handle and - left ill-typed" in {
    assertThrows[IllTypedException] {
      empty.typeof(AndExp(IntegerExp(42), TrueExp), Map())
    }
  }

  it should "handle and - right ill-typed" in {
    assertThrows[IllTypedException] {
      empty.typeof(AndExp(TrueExp, IntegerExp(42)), Map())
    }
  }

  it should "handle plus - well-typed" in {
    assertResult(IntType) {
      empty.typeof(PlusExp(IntegerExp(1), IntegerExp(2)), Map())
    }
  }

  it should "handle plus - left ill-typed" in {
    assertThrows[IllTypedException] {
      empty.typeof(PlusExp(TrueExp, IntegerExp(2)), Map())
    }
  }

  it should "handle plus - right ill-typed" in {
    assertThrows[IllTypedException] {
      empty.typeof(PlusExp(IntegerExp(1), TrueExp), Map())
    }
  }

  it should "handle less than - well-typed" in {
    assertResult(BoolType) {
      empty.typeof(LessThanExp(IntegerExp(1), IntegerExp(2)), Map())
    }
  }

  it should "handle less than - left ill-typed" in {
    assertThrows[IllTypedException] {
      empty.typeof(LessThanExp(TrueExp, IntegerExp(2)), Map())
    }
  }

  it should "handle less than - right ill-typed" in {
    assertThrows[IllTypedException] {
      empty.typeof(LessThanExp(IntegerExp(1), TrueExp), Map())
    }
  }

  it should "handle let - well-typed" in {
    assertResult(Map(x -> IntType)) {
      empty.typecheckStatement(LetStmt(x, IntType, IntegerExp(1)), Map())
    }
  }

  it should "handle let - shadowing" in {
    assertResult(Map(x -> IntType)) {
      empty.typecheckStatement(LetStmt(x, IntType, IntegerExp(1)), Map(x -> BoolType))
    }
  }

  it should "handle let - ill-typed" in {
    assertThrows[IllTypedException] {
      empty.typecheckStatement(LetStmt(x, IntType, TrueExp), Map())
    }
  }

  it should "handle assign - well-typed" in {
    assertResult(Map(x -> IntType)) {
      empty.typecheckStatement(AssignStmt(x, IntegerExp(1)), Map(x -> IntType))
    }
  }

  it should "handle assign - ill-typed (wrong type)" in {
    assertThrows[IllTypedException] {
      empty.typecheckStatement(AssignStmt(x, IntegerExp(1)), Map(x -> BoolType))
    }
  }

  it should "handle assign - ill-typed (not in scope)" in {
    assertThrows[IllTypedException] {
      empty.typecheckStatement(AssignStmt(x, IntegerExp(1)), Map())
    }
  }

  it should "handle higher-order function definition" in {
    assertResult(FunctionType(Seq(BoolType), IntType)) {
      empty.typeof(HigherOrderDefExp(Seq((BoolType -> x)), IntegerExp(1)), Map())
    }
  }

  it should "handle higher-order function calls" in {
    assertResult(IntType) {
      empty.typeof(
        HigherOrderCallExp(
          HigherOrderDefExp(Seq((IntType -> x)), VariableExp(x)),
          Seq(IntegerExp(0))),
        Map())
    }
  }

  it should "reject higher-order calls with parameter number mismatch" in {
    assertThrows[IllTypedException] {
      empty.typeof(
        HigherOrderCallExp(
          HigherOrderDefExp(Seq((IntType -> x)), VariableExp(x)),
          Seq(IntegerExp(0), IntegerExp(0))),
        Map())
    }
  }

  it should "reject higher-order calls with parameter type mismatch" in {
    assertThrows[IllTypedException] {
      empty.typeof(
        HigherOrderCallExp(
          HigherOrderDefExp(Seq((IntType -> x)), VariableExp(x)),
          Seq(TrueExp)),
        Map())
    }
  }

  val fn = FunctionName("id")
  val id = Function(IntType, fn, Seq((IntType -> x)), Seq(), VariableExp(x))
  val idChecker = Typechecker(Program(Seq(id)))

  it should "handle first-order calls" in {
    assertResult(IntType) {
      idChecker.typeof(FirstOrderCallExp(fn, Seq(IntegerExp(1))), Map())
    }
  }

  it should "reject first-order calls with the wrong number of params" in {
    assertThrows[IllTypedException] {
      idChecker.typeof(FirstOrderCallExp(fn, Seq(IntegerExp(1), IntegerExp(1))), Map())
    }
  }

  it should "reject first-order calls with the wrong types of params" in {
    assertThrows[IllTypedException] {
      idChecker.typeof(FirstOrderCallExp(fn, Seq(TrueExp)), Map())
    }
  }

  it should "accept well-typed programs" in {
    assertResult(()) {
      val fn = FunctionName("retTrue")
      val retTrue = Function(BoolType, fn, Seq(), Seq(), TrueExp)
      Typechecker(Program(Seq(id, retTrue)))
      ()
    }
  }

  it should "reject programs with duplicate function names" in {
    assertThrows[IllTypedException] {
      Typechecker(Program(Seq(id, id)))
    }
  }

  it should "reject programs with duplicate first-order function formal params" in {
    assertThrows[IllTypedException] {
      Typechecker(Program(Seq(
        Function(
          BoolType,
          FunctionName("junk"),
          Seq((IntType -> x), (IntType -> x)),
          Seq(),
          TrueExp))))
    }
  }
} // TestTypechecker
