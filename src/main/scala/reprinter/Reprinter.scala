package reprinter

sealed trait RefactorType
case object Before extends RefactorType
case object After extends RefactorType
case object Replace extends RefactorType


case class Position(line: Int, col: Int)
case class Span(start: Position, end: Position)