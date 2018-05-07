package ca.valencik.antlr4fun

import org.antlr.v4.runtime._
import scala.util.Try

import scala.collection.JavaConversions._

sealed trait Expr
case class Expression(left: Double, right: Double, op: Operation) extends Expr
case class Operation(op: Op)                                      extends Expr
object Operation {
  def apply(name: String): Operation = {
    val op = name match {
      case "ADD" => ADD
      case "SUB" => SUB
      case "MUL" => MUL
      case "DIV" => DIV
    }
    new Operation(op)
  }
}

sealed trait Op
case object ADD extends Op
case object MUL extends Op
case object SUB extends Op
case object DIV extends Op

class ArithmeticVisitorApp extends ArithmeticParserBaseVisitor[Expr] {

  override def visitExpr(ctx: ArithmeticParser.ExprContext): Expression = {
    val exprText: String = ctx.getText

    val operands = ctx.NUMBER().toList.map(_.getText)
    val operand1 = parseDouble(operands.lift(0).getOrElse("0.0")).getOrElse(0.0)
    val operand2 = parseDouble(operands.lift(1).getOrElse("0.0")).getOrElse(0.0)

    val operation = visitOperation(ctx.operation())

    Expression(operand1, operand2, operation)
  }

  override def visitOperation(
      ctx: ArithmeticParser.OperationContext): Operation = {
    val op = ArithmeticParser.VOCABULARY.getSymbolicName(ctx.getStart.getType)
    Operation(op)
  }

  def parseDouble(s: String): Option[Double] = Try(s.toDouble).toOption
}

object Calculator {

  def parse(input: String): Expression = {
    val charStream = new ANTLRInputStream(input)
    val lexer      = new ArithmeticLexer(charStream)
    val tokens     = new CommonTokenStream(lexer)
    val parser     = new ArithmeticParser(tokens)

    val arithmeticVisitor = new ArithmeticVisitorApp()
    val res               = arithmeticVisitor.visit(parser.expr())
    res.asInstanceOf[Expression]
  }

  def evaluate(exp: Expression): Option[Double] = {
    val left  = exp.left
    val right = exp.right
    exp.op.op match {
      case ADD => Some(left + right)
      case SUB => Some(left - right)
      case MUL => Some(left * right)
      case DIV => Try(left / right).toOption
      case _ =>
        println(s"Unsupported operation")
        None
    }
  }
}

object CalculatorApp extends App {
  import ca.valencik.antlr4fun.Calculator._

  val exp: String            = "1 + 1"
  val parsedExp: Expression  = parse(exp)
  val result: Option[Double] = evaluate(parsedExp)
  println(exp, parsedExp, result)
}
