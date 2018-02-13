package reprinter

import org.scalatest.FunSuite
import org.langmeta.inputs.{Input, Position}
import System.{lineSeparator => nl}

case class Ast(decls: List[Decl]) {
  override def toString: String = decls.mkString(nl)
}
case class Decl(pos: Position, name: String, expr: Expr) {
  override def toString: String = s"$name = $expr"
}

sealed trait Expr
case class Plus(refactored: Boolean, pos: Position, lhs: Expr, rhs: Expr) extends Expr {
  override def toString: String = s"+($lhs, $rhs)"
}
case class Var(refactored: Boolean, pos: Position, name: String) extends Expr {
  override def toString: String = name
}
case class Const(refactored: Boolean, pos: Position, value: Int) extends Expr {
  override def toString: String = value.toString
}

object RefactorZero extends RefactorUtils {
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

    def markRefactored(expr: Expr, pos: Position): Expr = {
      expr match {
        case Plus(_, _, e1, e2) => Plus(true, pos, e1, e2)
        case Var(_, _, n) => Var(true, pos, n)
        case Const(_, _, i) => Const(true, pos, i)
      }
    }
    ast.copy(decls = ast.decls.map(d => d.copy(expr = go(d.expr))))
  }
}

class ReprinterSuite() extends FunSuite {
  val input = 
    """|x = +(1,2)
       |y = +(x, 0)
       |// Calculate z
       |z = +( 1, +(+(0,x) ,y) )""".stripMargin

    
  val ast0 = new AstParser(input).result
  val ast = RefactorZero(ast0)

  println(ast0)
  println()
  println(ast)
}

class AstParser(input: String) {
  val IgnoreSpaces = fastparse.WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import IgnoreSpaces._

  val metaInput = Input.String(input)
  def pos(startOffset: Int, endOffset: Int): Position =
    Position.Range(metaInput, startOffset, endOffset)
  val number: P[Int] = P(CharIn('0'to'9').rep(1).!.map(_.toInt))
  val name: P[String] = P(CharIn('a' to 'z').rep(1).!)
  def wrapPos[A, B](p: P[A])(f: (A, Position) => B): P[B] =
    (Index ~ p ~ Index).map {case (start, v, end) => f(v, pos(start, end))}
  val const: P[Const] = wrapPos(number){ case (v, pos) => Const(false, pos, v) }
  val `var`: P[Var] = wrapPos(name){ case (v, pos) => Var(false, pos, v)}
  val plus: P[Plus] = wrapPos(P("+(" ~ expr ~ "," ~ expr ~ ")")){
    case ((e1, e2), pos) => Plus(false, pos, e1, e2)
  }
  val expr: P[Expr] = const | `var` | plus
  val decl: P[Decl] = wrapPos(name ~ "=" ~ expr){
    case ((n, e), pos) => Decl(pos, n, e)
  }
  val comment: P[Unit] = P("//" ~ (!(nl) ~ AnyChar).rep).map(_ => ())

  val ast: P[Ast] = 
    (Start ~ (decl | comment).rep(sep = nl) ~ End).map(decls => 
      Ast(decls.collect{ case d: Decl => d }.toList)
    )

  def result: Ast = ast.parse(input).get.value
}

trait RefactorUtils {
  def refactorLoop(refactoring: Ast => Ast)(ast: Ast): Ast = {
    val ast0 = refactoring(ast)
    if (ast0 == ast) ast
    else refactorLoop(refactoring)(refactoring(ast0))
  }
}

