package expressions_and_statements

import org.scalatest.FlatSpec

class TestTypechecker extends FlatSpec {
  import Typechecker._

  val x = Variable("x")
  val y = Variable("y")

  "The typechecker" should "handle variables in scope" in {
    assertResult(IntType) {
      typeof(VariableExp(x), Map(x -> IntType))
    }
  }

  it should "handle variables not in scope" in {
    assertThrows[IllTypedException] {
      typeof(VariableExp(x), Map())
    }
  }

  it should "handle integers" in {
    assertResult(IntType) {
      typeof(IntegerExp(42), Map())
    }
  }

  it should "handle true" in {
    assertResult(BoolType) {
      typeof(TrueExp, Map())
    }
  }

  it should "handle false" in {
    assertResult(BoolType) {
      typeof(FalseExp, Map())
    }
  }

  it should "handle and - well-typed" in {
    assertResult(BoolType) {
      typeof(AndExp(TrueExp, TrueExp), Map())
    }
  }

  it should "handle and - left ill-typed" in {
    assertThrows[IllTypedException] {
      typeof(AndExp(IntegerExp(42), TrueExp), Map())
    }
  }

  it should "handle and - right ill-typed" in {
    assertThrows[IllTypedException] {
      typeof(AndExp(TrueExp, IntegerExp(42)), Map())
    }
  }

  it should "handle plus - well-typed" in {
    assertResult(IntType) {
      typeof(PlusExp(IntegerExp(1), IntegerExp(2)), Map())
    }
  }

  it should "handle plus - left ill-typed" in {
    assertThrows[IllTypedException] {
      typeof(PlusExp(TrueExp, IntegerExp(2)), Map())
    }
  }

  it should "handle plus - right ill-typed" in {
    assertThrows[IllTypedException] {
      typeof(PlusExp(IntegerExp(1), TrueExp), Map())
    }
  }

  it should "handle less than - well-typed" in {
    assertResult(BoolType) {
      typeof(LessThanExp(IntegerExp(1), IntegerExp(2)), Map())
    }
  }

  it should "handle less than - left ill-typed" in {
    assertThrows[IllTypedException] {
      typeof(LessThanExp(TrueExp, IntegerExp(2)), Map())
    }
  }

  it should "handle less than - right ill-typed" in {
    assertThrows[IllTypedException] {
      typeof(LessThanExp(IntegerExp(1), TrueExp), Map())
    }
  }

  it should "handle let - well-typed" in {
    assertResult(Map(x -> IntType)) {
      typecheckStatement(LetStmt(x, IntType, IntegerExp(1)), Map())
    }
  }

  it should "handle let - shadowing" in {
    assertResult(Map(x -> IntType)) {
      typecheckStatement(LetStmt(x, IntType, IntegerExp(1)), Map(x -> BoolType))
    }
  }

  it should "handle let - ill-typed" in {
    assertThrows[IllTypedException] {
      typecheckStatement(LetStmt(x, IntType, TrueExp), Map())
    }
  }

  it should "handle assign - well-typed" in {
    assertResult(Map(x -> IntType)) {
      typecheckStatement(AssignStmt(x, IntegerExp(1)), Map(x -> IntType))
    }
  }

  it should "handle assign - ill-typed (wrong type)" in {
    assertThrows[IllTypedException] {
      typecheckStatement(AssignStmt(x, IntegerExp(1)), Map(x -> BoolType))
    }
  }

  it should "handle assign - ill-typed (not in scope)" in {
    assertThrows[IllTypedException] {
      typecheckStatement(AssignStmt(x, IntegerExp(1)), Map())
    }
  }

  it should "handle programs - single statement" in {
    assertResult(Map(x -> IntType)) {
      typecheckProgram(SingleStatement(LetStmt(x, IntType, IntegerExp(1))), Map())
    }
  }

  it should "handle programs - multi statement" in {
    assertResult(Map(x -> IntType, y -> BoolType)) {
      val prog =
        MultiStatement(
          LetStmt(x, IntType, IntegerExp(1)),
          SingleStatement(
            LetStmt(y, BoolType, TrueExp)))
      typecheckProgram(prog, Map())
    }
  }
} // TestTypechecker
