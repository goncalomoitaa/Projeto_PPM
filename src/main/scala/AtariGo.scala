import AtariGo.Stone.{ Empty, Stone }

import scala.annotation.tailrec
import scala.util.Random

object AtariGo {

  type Board = List[List[Stone]]
  type Coord2D = (Int, Int)       //(row, column)

  object Stone extends Enumeration {
    type Stone = Value
    type Captured = Boolean
    val Black, White, Empty = Value
  }

  trait Random { //interface Random
    def nextInt(x: Int): (Int, MyRandom)
  }

  case class MyRandom(seed: Long) extends Random {
    def nextInt(x: Int): (Int, MyRandom) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRandom = MyRandom(newSeed)
      val n = (newSeed >>> 16).toInt % x //x é o tamanho máximo permitido
      (if (n < 0) -n else n, nextRandom) //caso o n seja negativo transforma em positivo
    }
  }

  def initializeBoard(size: Int): Board = {
    List.fill(size, size)(Stone.Empty)
  }

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val (randInt, newRand) = rand.nextInt(lstOpenCoords.length)
    (lstOpenCoords(randInt), newRand)
  }

  def isElementUnused(coord : Coord2D, lstOpenCoords : List[Coord2D]) : Boolean = {
    (lstOpenCoords foldRight false)(
      (x, r) => if(x == coord) true else r )
  }

  def lengthBoard(board : Board): Int = {
    (board foldLeft 0)((r, x) => 1 + r)
  }

  def filterBoard(lstOpenCoords : List[Coord2D], coord : Coord2D) : List[Coord2D] = lstOpenCoords match {
    case Nil => Nil
    case head :: tail =>
      if(head == coord)
        filterBoard(tail, coord)
      else
        head :: filterBoard(tail, coord)
  }

  //perguntar se podemos usar filternot?
  //  T2
  def play(board: Board, player: Stone, coord: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
    if isElementUnused( coord, lstOpenCoords ) then
      val (row, column) = coord
      var newBoard = List.fill( lengthBoard( board ), lengthBoard( board ) )( Stone.Empty )
      newBoard = board.updated( row, board( row ).updated( column, player ) ) // podemos usar a funcao updated??
          ( Some( newBoard ), filterBoard( lstOpenCoords, coord ) )
    else
      (None, lstOpenCoords)
  }

  //  T3
  //  prof diz que podemos adicionar a posicao que foi jogada na tuple retornada.
  //  util para implementar 'Undo'
  def playRandomly(board:Board, r:MyRandom, player:Stone, lstOpenCoords:List[Coord2D],
                   f:(List[Coord2D], MyRandom) => (Coord2D,MyRandom)
                  ) : (Board,MyRandom,List[Coord2D]) =
  {
    //        val (newCoord2D, MyRandomInstance) = randomMove(lstOpenCoords, r)
    val (newCoord2D, myRandomInstance) = f(lstOpenCoords, r)
    val (newBoardRes, updatedLstOpenCoords) = play(board, player, newCoord2D, lstOpenCoords)
    newBoardRes match {
      case Some(newBoard) =>
        (newBoard, myRandomInstance, updatedLstOpenCoords)
      case None => (board, myRandomInstance, lstOpenCoords)
    }
  }

  def getEmptyCoordsList(board: Board): List[Coord2D] = {
    
    def gatherEmptyCoords(lst:List[Stone], currRow:Int, acc:Int) : List[Coord2D] = lst match{
      case Nil ⇒ Nil
      case head :: tail ⇒
        if(head != Stone.Empty) gatherEmptyCoords(tail, currRow, acc + 1)
        else (currRow, acc) :: gatherEmptyCoords(tail, currRow, acc + 1)
    }
    
    def gatherLists(board:Board, acc:Int) : List[Coord2D] = board match {
      case Nil => Nil
      case head :: tail => gatherEmptyCoords(head, acc, 0) ++ gatherLists(tail, acc + 1)
    }
    
    gatherLists(board, 0)
  }

  //  T4
  @tailrec
  def visualizeBoard(board: Board): Unit = board match {

    case Nil => ()
    case head :: tail =>
      visualizeLine(head)
      println()
      visualizeBoard(tail)
  }
  
  @tailrec
  def visualizeLine(line: List[Stone]): Unit = line match {
    case Nil => ()
    case head :: tail =>
      head match {
        case Stone.Empty => print(" . ")
        case Stone.Black => print(" B ")
        case Stone.White => print(" W ")
      }
      visualizeLine(tail)
  }


  def main(args : Array[String]) : Unit = {
    val board = List(
        List(Stone.Black, Stone.Empty),
        List(Stone.White, Stone.Empty),
        List(Stone.White, Stone.Empty),
        List(Stone.White, Stone.Empty))
    print("Empty coords: ")
    val lstEmptyCoords = getEmptyCoordsList( board )
    println( lstEmptyCoords )
    
    val random = MyRandom(System.currentTimeMillis())
    val f = randomMove(_: List[Coord2D], _ :MyRandom)
    val (b, r, newLstOpenCoords) = playRandomly(board, random, Stone.Black, lstEmptyCoords, f)
    val i = MyRandom(System.currentTimeMillis())
    val (move, newRand) = randomMove(newLstOpenCoords, i)
    val (newBoard, updatedLstOpenCoords) = play(board, Stone.Black, move, newLstOpenCoords)
    println("Board after move:")
    newBoard match {
      case Some(b) => visualizeBoard(b)
      case None => println("Invalid move")
    }
  }
}
