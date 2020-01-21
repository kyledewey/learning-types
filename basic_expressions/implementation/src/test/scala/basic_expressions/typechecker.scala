package basic_expressions

import org.scalatest.FlatSpec

class TestTypechecker extends FlatSpec {
  import Typechecker.typeof

  val x = Variable("x")

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
    assertResult(IntType) {
      typeof(LetExp(x, IntType, IntegerExp(1), VariableExp(x)), Map())
    }
  }

  it should "handle let - shadowing" in {
    assertResult(IntType) {
      typeof(LetExp(x, IntType, IntegerExp(1), VariableExp(x)), Map(x -> BoolType))
    }
  }

  it should "handle let - ill-typed" in {
    assertThrows[IllTypedException] {
      typeof(LetExp(x, IntType, TrueExp, VariableExp(x)), Map(x -> BoolType))
    }
  }

  it should "handle assign - well-typed" in {
    assertResult(IntType) {
      typeof(AssignExp(x, IntegerExp(1), VariableExp(x)), Map(x -> IntType))
    }
  }

  it should "handle assign - ill-typed (wrong type)" in {
    assertThrows[IllTypedException] {
      typeof(AssignExp(x, IntegerExp(1), VariableExp(x)), Map(x -> BoolType))
    }
  }

  it should "handle assign - ill-typed (not in scope)" in {
    assertThrows[IllTypedException] {
      typeof(AssignExp(x, IntegerExp(1), VariableExp(x)), Map())
    }
  }
}
