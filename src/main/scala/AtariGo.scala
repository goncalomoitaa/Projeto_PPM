import AtariGo.Stone.Stone

object AtariGo {

  type Board = List[List[Stone]]
  type Coord2D = (Int, Int)       //(row, column)

  object Stone extends Enumeration {
    type Stone = Value
//    type Captured = Boolean ????
    val Black, White, Empty = Value
  }

  trait Random {                         //interface Random
    def nextInt(x : Int) : (Int, Random)
  }

  case class MyRandom(seed : Long) extends Random {
    def nextInt(x : Int): (Int , Random) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRandom = MyRandom(newSeed)
      val n = (newSeed >>> 16).toInt % x                           //x é o tamanho máximo permitido
      (if (n < 0) -n else n, nextRandom)                             //caso o n seja negativo transforma em positivo
    }
  }

  def randomMove(lstOpenCoords : List[Coord2D], rand : Random) : (Coord2D, Random) = {
    val (randInt, newRand) = rand.nextInt(lstOpenCoords.length)
    (lstOpenCoords(randInt), newRand)
  }

  def isElementUnused(coord : Coord2D, lstOpenCoords : List[Coord2D]) : Boolean = {
    (lstOpenCoords foldRight false)((x, r) =>
      if(x == coord)
        true
      else
        r
    )
  }

  def lengthBoard(board : Board): Int = {
    (board foldLeft 0)((r, x) => 1 + r)
  }

  def filterBoard(lstOpenCoords : List[Coord2D], coord : Coord2D) : List[Coord2D] = lstOpenCoords match {
    case Nil => Nil
    case primeiro :: tail =>
      if(primeiro == coord)
        filterBoard(tail, coord)
      else
        primeiro :: filterBoard(tail, coord)
  }
  
  //perguntar se podemos usar filternot???
  def play(board : Board, player : Stone, coord : Coord2D, lstOpenCoords : List[Coord2D]) : (Option[Board], List[Coord2D]) = {
    if(!isElementUnused(coord, lstOpenCoords))
      (None, lstOpenCoords)
    else
      val (row, column) = coord
      var newBoard = List.fill(lengthBoard(board), lengthBoard(board))(Stone.Empty)
      newBoard = board.updated(row, board(row).updated(column, player))
      (Some(newBoard), filterBoard(lstOpenCoords, coord))
  }

  def main(args : Array[String]) : Unit = {
    val k = List((0,0), (0,1), (1,1))
    val c = (1,1)
    val i = MyRandom(System.currentTimeMillis())
    val (move, newRand) = randomMove(k, i)
    val board = List(List(Stone.Black, Stone.Empty), List(Stone.White, Stone.Empty))
    val lst = List((0,1), (1,1))
//    println(play(board, Stone.Black, (0,1), lst))
//    println(s"Random move: $move")
//    println(newRand)
//    print(List.fill(9, 9)(Stone.Empty))
//    println(lengthBoard(board))
  }
}