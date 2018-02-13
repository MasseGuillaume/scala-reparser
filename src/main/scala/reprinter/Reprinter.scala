package reprinter

sealed trait RefactorType
case object Before extends RefactorType
case object After extends RefactorType
case object Replace extends RefactorType