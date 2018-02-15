package reprinter

import org.scalatest.FunSuite
import org.langmeta.inputs.{Input, Position}
import System.{lineSeparator => nl}

case class Ast(isRefactored: Boolean, pos: Position, decls: List[Decl]) extends AstNode {
  override def toString: String = decls.mkString(nl)
}
case class Decl(isRefactored: Boolean, pos: Position, name: String, expr: Expr) extends AstNode {
  override def toString: String = s"$name = $expr"
}

sealed trait Expr extends AstNode
case class Plus(isRefactored: Boolean, pos: Position, lhs: Expr, rhs: Expr) extends Expr {
  override def toString: String = s"+($lhs, $rhs)"
}
case class Var(isRefactored: Boolean, pos: Position, name: String) extends Expr {
  override def toString: String = name
}
case class Const(isRefactored: Boolean, pos: Position, value: Int) extends Expr {
  override def toString: String = value.toString
}

trait Refactorer {
  def name: String
  def apply(ast: Ast): Ast

  protected def refactorLoop(refactoring: Ast => Ast)(ast: Ast): Ast = {
    val ast0 = refactoring(ast)
    if (ast0 == ast) ast
    else refactorLoop(refactoring)(refactoring(ast0))
  }
}

object RefactorZero extends Refactorer {
  def name: String = "RefactorZero"
  def apply(ast: Ast): Ast = refactorLoop(refactorZeroOnce)(ast)
  private def refactorZeroOnce(ast: Ast): Ast = {
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
    ast.copy(
      decls = ast.decls.map(d => d.copy(expr = go(d.expr)))
    )
  }
}

object Reorder extends Refactorer {
  def name: String = "Reorder"
  def apply(ast: Ast): Ast = {
    val decls = 
      ast.decls match {
        case List(e1, d1: Decl, d2: Decl, e4) => 
          List(
            e1,
            d2.copy(pos = d1.pos, isRefactored = true),
            d1.copy(pos = d2.pos, isRefactored = true),
            e4
          )
        case es => es
      }
    ast.copy(decls = decls)
  }
}

object ExampleReprinter {
  private def traversal(root: AstNode)(f: AstNode => Unit): Unit = {
    def loop(node: AstNode): Unit = {
      if (node.isRefactored) f(node)
      else {
        node match {
          case Ast(_, _, decls)     => decls.foreach(loop)
          case Decl(_, _, _, expr)  => loop(expr)
          case Plus(_, _, lhs, rhs) => loop(lhs); loop(rhs)
          case Var(_, _, _)         => ()
          case Const(_, _, _)       => ()
        }
      }
    }
    loop(root)
  }
  private def prettyPrint(node: AstNode): String = node.toString

  def apply(ast: AstNode, input: String): String =
    Reprinter(prettyPrint, traversal _, ast, input)
}

class ReprinterSuite() extends FunSuite {
  refactor(Reorder)(
    """|a = 1
       |  b = 2
       |c = 3
       |d = 4""".stripMargin,
    """|a = 1
       |  c = 3
       |b = 2
       |d = 4""".stripMargin
  )

  refactor(RefactorZero)(
    """|x = +(1,2)
       |y = +(x, 0)
       |// Calculate z
       |z = +( 1, +(+(0,x) ,y) )""".stripMargin,
    """|x = +(1,2)
       |y = x
       |// Calculate z
       |z = +( 1, +(x,y) )""".stripMargin
  )

  def refactor(refactorer: Refactorer)(input: String, expected: String): Unit = {
    test(refactorer.name){
      val obtained = ExampleReprinter(refactorer(new AstParser(input).result), input)
      assert(obtained == expected)
    }
  } 
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
    case ((n, e), pos) => Decl(false, pos, n, e)
  }
  val comment: P[Unit] = P("//" ~ (!(nl) ~ AnyChar).rep).map(_ => ())

  val ast: P[Ast] = 
    (Start ~ (decl | comment).rep(sep = nl) ~ End).map{declsAndComments => 
      val decls = declsAndComments.collect{ case d: Decl => d }.toList
      Ast(false, pos(0, input.size), decls)
    }

  def result: Ast = ast.parse(input).get.value
}

trait RefactorUtils {
  
}

