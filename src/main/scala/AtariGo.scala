import AtariGo.Stone.{ Empty, Stone }

import scala.annotation.tailrec

object AtariGo {

  type Board = List[List[Stone]]
  type Coord2D = (Int, Int)       //(row, column)

  object Stone extends Enumeration {
    type Stone = Value
//    type Captured = Boolean
    val Black, White, Empty = Value
  }

  trait Random { //interface Random
    def nextInt(x: Int): (Int, MyRandom)
  }

  case class MyRandom(seed: Long) extends Random {
    def nextInt(x: Int): (Int, MyRandom) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRandom = MyRandom(newSeed)
      val n = (newSeed >>> 16).toInt % x
      (if (n < 0) -n else n, nextRandom) // in case 'n' is negative, return it as positive
    }
  }

  def initializeBoard(size: Int): Board = {
    List.fill(size, size)(Stone.Empty)
  }

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val (randInt, newRand) = rand.nextInt(lstOpenCoords.length)
    (lstOpenCoords(randInt), newRand)
  }

  def isCoordEmpty(coord : Coord2D, lstOpenCoords : List[Coord2D]) : Boolean = {
    (lstOpenCoords foldRight false)( (x, r) => if(x == coord) true else r )
  }

  def getBoardSize(board : Board): Int = {
    (board foldLeft 0)((r, x) => 1 + r)
  }

  def filterOutCoord(lstOpenCoords : List[Coord2D], targetCoord : Coord2D) : List[Coord2D] = lstOpenCoords match {
    case Nil => Nil
    case head :: tail =>
      if(head == targetCoord)
        filterOutCoord(tail, targetCoord)
      else
        head :: filterOutCoord(tail, targetCoord)
  }

  //  T2
  def play(board : Board, player : Stone, coord : Coord2D, lstOpenCoords : List[Coord2D]) : (Option[Board], List[Coord2D]) = {
    if(!isCoordEmpty(coord, lstOpenCoords))
      (None, lstOpenCoords) // returns None since move was invalid
    else
      val (row, column) = coord
      var newBoard = initializeBoard(getBoardSize(board))
      newBoard = board.updated(row, board(row).updated(column, player))   // Taken from (T2 - Lists, Recursion and Pattern Matching_v4.pdf)
      (Some(newBoard), filterOutCoord(lstOpenCoords, coord))
  }

  //  T3
  //  prof diz que podemos adicionar a posicao que foi jogada na tuple retornada.
  //  util para implementar 'Undo'
  def playRandomly(board:Board, r:MyRandom, player:Stone, lstOpenCoords:List[Coord2D],
                   f:(List[Coord2D], MyRandom) => (Coord2D,MyRandom)
                  ) : (Board,MyRandom,List[Coord2D]) =
  {
    val (newCoord2D, myRandomInstance) = f(lstOpenCoords, r)
    val (newBoardRes, updatedLstOpenCoords) = play(board, player, newCoord2D, lstOpenCoords)
    newBoardRes match {
      case Some(newBoard) =>
        (newBoard, r, updatedLstOpenCoords)
      case None => (board, r, lstOpenCoords)
    }
  }

  def getBoardEmptyCoords(board: Board): List[Coord2D] = {
    
    def getRowEmptyCoords(lst:List[Stone], currRow:Int, acc:Int) : List[Coord2D] = lst match{
      case Nil ⇒ Nil
      case head :: tail ⇒
        if(head != Stone.Empty) getRowEmptyCoords(tail, currRow, acc + 1)
        else (currRow, acc) :: getRowEmptyCoords(tail, currRow, acc + 1)
    }
    
    def loopRowLists(board:Board, acc:Int) : List[Coord2D] = board match {
      case Nil => Nil
      case head :: tail => getRowEmptyCoords(head, acc, 0) ++ loopRowLists(tail, acc + 1)
    }
    
    loopRowLists(board, 0)
  }
  
  //  T4
  @tailrec
  def drawBoard(board: Board): Unit = {
    
    @tailrec
    def drawLine(line: List[ Stone ]): Unit = line match {
      case Nil => ()
      case head :: tail =>
        head match {
          case Stone.Empty => print( " . " )
          case Stone.Black => print( " B " )
          case Stone.White => print( " W " )
        }
        drawLine( tail )
    }
    
    board match {
      case Nil => ()
      case head :: tail =>
        drawLine( head )
        println()
        drawBoard( tail )
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val emptyBoardExample = initializeBoard(4)
    
    val exampleBoard = List(
        List(Stone.Black, Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty),
        List(Stone.White, Stone.Empty, Stone.Black, Stone.Empty, Stone.Empty),
        List(Stone.Empty, Stone.Empty, Stone.Empty, Stone.White, Stone.Empty),
        List(Stone.White, Stone.Empty, Stone.Black, Stone.Empty, Stone.Empty),
        List(Stone.Empty, Stone.Empty, Stone.Empty, Stone.White, Stone.Empty)
    )
    
    print("Empty coords: ")
    val lstEmptyCoords = getBoardEmptyCoords( exampleBoard )
    println( lstEmptyCoords )
    
    println("Board before move:")
    drawBoard(exampleBoard)
    
    val randomInstance = MyRandom(System.currentTimeMillis())
    
    val funcRandMovePlay = randomMove(_: List[Coord2D], _ :MyRandom)  //  randomMove function as value
    val (boardFirstMove, r, newLstOpenCoords1) = playRandomly(exampleBoard, randomInstance, Stone.Black, lstEmptyCoords, funcRandMovePlay)
    
    println("Board after first move (playRandomly):")
    drawBoard(boardFirstMove)
    
    //val i = MyRandom(System.currentTimeMillis())  //  we do not want a new instance of MyRandom, we want to keep the same instance
                                                    //  for the entirety of this exampleBoard instance
    
    val (randMove, newRand) = randomMove(newLstOpenCoords1, randomInstance)
    val (boardSecondMove, newLstOpenCoords2) = play(boardFirstMove, Stone.Black, randMove, newLstOpenCoords1)
    
    println("Board after second move (randomMove + play):")
    boardSecondMove match {
      case Some(boardSecondMove) => drawBoard(boardSecondMove)
      case None => println( "Invalid move" )
    }

//    println("")
//
//    drawBoard(emptyBoardExample)
  }
}
