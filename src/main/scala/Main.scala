type Board = List[List[Stone]]

type Coord2D = (Int, Int) //(row, column)

object Stone extends Enumeration {
  type Stone = Value
  val Black, White, Empty = Value
}