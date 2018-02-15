package reprinter

import org.langmeta.inputs.Position

sealed trait RefactorType
case object Before extends RefactorType
case object After extends RefactorType
case object Replace extends RefactorType

trait AstNode {
  def isRefactored: Boolean
  def pos: Position
}

object Reprinter {
  def apply(prettyPrint: AstNode => String, 
            traversal: AstNode => (AstNode => Unit) => Unit,
            refactored: AstNode,
            source: String): String = {

    var cursor = 0
    val buf = new StringBuffer
    traversal(refactored){ node =>
      if(node.isRefactored) {
        buf.append(source, cursor, node.pos.start)
        buf.append(prettyPrint(node))
        cursor = node.pos.end
      }
    }
    buf.append(source, cursor, source.size)
    buf.toString
  }
}