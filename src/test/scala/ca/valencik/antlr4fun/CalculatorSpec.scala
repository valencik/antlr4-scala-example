package ca.valencik.antlr4fun

import ca.valencik.antlr4fun.Calculator._
import org.scalatest._

class CalculatorSpec extends FlatSpec with Matchers {
  // Setup === and +- for handling comparison of Doubles
  import org.scalactic._
  import TolerantNumerics._
  import TripleEquals._
  implicit val dblEquality = tolerantDoubleEquality(0.01)

  "Calculator parse" should "work with weird spacing" in {
    parse("0 +      0") shouldBe Expression(0, 0, Operation(ADD))
    parse("1.1     -2") shouldBe Expression(1.1, 2, Operation(SUB))
    parse("3      * 4") shouldBe Expression(3, 4, Operation(MUL))
    parse(" 5.5  / 6 ") shouldBe Expression(5.5, 6, Operation(DIV))
  }

  "Calculator evaluate" should "evaluate addition" in {
    evaluate(parse("0 +      0")) shouldEqual Some(0)
    evaluate(parse("1     +  0")) shouldEqual Some(1)
    evaluate(parse("  1 + 1   ")) shouldEqual Some(2)
  }

  it should "evaluate subtraction" in {
    evaluate(parse("0 - 0")) shouldEqual Some(0)
    evaluate(parse("1 - 0")) shouldEqual Some(1)
    evaluate(parse("1 - 1")) shouldEqual Some(0)
    evaluate(parse("9 - 1")) shouldEqual Some(8)
    evaluate(parse("1 - 9")) shouldEqual Some(-8)
  }

  it should "evaluate multiplication" in {
    evaluate(parse("1 * 0")) shouldEqual Some(0)
    evaluate(parse("1 * 1")) shouldEqual Some(1)
    assert(evaluate(parse("1.1 * 2")) === Some(2.2))
  }

  it should "evaluate division" in {
    evaluate(parse("1 / 1")) shouldEqual Some(1)
    evaluate(parse("2 / 1")) shouldEqual Some(2)
    assert(evaluate(parse("5.5 / 1.1")) === Some(5.0))
  }
}
