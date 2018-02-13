package reprinter

import org.scalatest.FunSuite

import org.langmeta.inputs.Input

import fastparse.all._

case class Ast(decls: List[Decl])
case class Decl(span: Span, name: String, expr: Expr)

sealed trait Expr
case class Plus(refactored: Boolean, span: Span, lhs: Expr, rhs: Expr) extends Expr
case class Var(refactored: Boolean, span: Span, name: String) extends Expr
case class Const(refactored: Boolean, span: Span, value: Int) extends Expr

class AstParser(input: String) {
  val metaInput = Input.String(input)
  def pos(offset: Int): Position = ???
  def span(startOffset: Int, endOffset: Int): Span = Span(pos(startOffset), pos(endOffset))
  val number: P[Int] = P(CharIn('0'to'9').rep(1).!.map(_.toInt))
  val name: P[String] = P(CharIn('a' to 'z').rep(1).!)
  def wrapSpan[A, B](p: P[A])(f: (A, Span) => B): P[B] =
    (Index ~ p ~ Index).map {case (start, v, end) => f(v, span(start, end))}
  val const: P[Const] = wrapSpan(number){ case (v, s) => Const(false, s, v) }
  val `var`: P[Var] = wrapSpan(name){ case (v, s) => Var(false, s, v)}
  val plus: P[Plus] = wrapSpan(P("+(" ~ expr ~ "," ~ expr ~ ")")){
    case ((e1, e2), s) => Plus(false, s, e1, e2)
  }
  val expr: P[Expr] = const | `var` | plus
  val decl: P[Decl] = wrapSpan(name ~ "=" ~ expr){
    case ((n, e), s) => Decl(s, n, e)
  }
  val ast: P[Ast] = decl.rep.map(decls => Ast(decls.toList))

  def result: Ast = ast.parse(input).get.value
}

class ReprinterSuite() extends FunSuite {
  val input = 
    """|x = +(1,2)
       |y = +(x, 0)
       |// Calculate z
       |z = +( 1, +(+(0,x) ,y) )""".stripMargin

  val ast = RefactorZero(new AstParser(input).result)
}

object RefactorZero extends RefactorUtils{
  def apply(ast: Ast): Ast = refactorLoop(refactorZeroOnce)(ast)
  def refactorZeroOnce(ast: Ast): Ast = {
    def go(expr: Expr): Expr = {
      expr match {
        case Plus(_, s, e, Const(_, _, 0)) => markRefactored(go(e), s)
        case Plus(_, s, Const(_, _, 0), e) => markRefactored(go(e), s)
        case Plus(b, s, e1, e2) => Plus(b, s, go(e1), go(e2))
        case e => e
      }
    }

    def markRefactored(expr: Expr, s: Span): Expr = {
      expr match {
        case Plus(_, _, e1, e2) => Plus(true, s, e1, e2)
        case Var(_, _, n) => Var(true, s, n)
        case Const(_, _, i) => Const(true, s, i)
      }
    }
    ast.copy(decls = ast.decls.map(d => d.copy(expr = go(d.expr))))
  }
}

trait RefactorUtils {
  def refactorLoop(refactoring: Ast => Ast)(ast: Ast): Ast = {
    val ast0 = refactoring(ast)
    if (ast0 == ast) ast
    else refactorLoop(refactoring)(refactoring(ast0))
  }
}