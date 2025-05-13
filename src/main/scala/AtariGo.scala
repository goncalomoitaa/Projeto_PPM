import AtariGo.Stone.{ Empty, Stone }

import scala.annotation.tailrec
import scala.collection.immutable.::

import MyRandom._
import AtariGo.Stone.{ Empty, Stone }
import TUI_Utils.{getUserInput, printGameOver, printGameState, printableFlipResult, showPrompt, tossCoin}

case class GameState(numFlips: Int, numCorrect: Int)

object AtariGo extends App{

  type Board = List[List[Stone]]
  type Coord2D = (Int, Int)       //(row, column)

  object Stone extends Enumeration {
    type Stone = Value
    //    type Captured = Boolean
    val Black, White, Empty = Value
  }

  def getOppositeStone(player:Stone) : Stone =
    if (player == Stone.Black) Stone.White else Stone.Black
  
  def initializeBoard(size: Int): Board = {  //cria uma board que tem o mesmo nº de linhas como de colunas, toda vazia
    List.fill(size, size)(Stone.Empty)
  }

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val (randInt, newRand) = rand.nextInt(lstOpenCoords.length) //gera uma coordenada aleatória e um rand novo
    (lstOpenCoords(randInt), newRand) //devolve uma coordenada aleatória da lista de coordenadas vazias e o new rand
  }

  def isCoordEmpty(coord : Coord2D, lstOpenCoords : List[Coord2D]) : Boolean = {
    (lstOpenCoords foldRight false)( (x, r) => if(x == coord) true else r ) //procura usando fold se a coordenada esta na lista de coordenadas vazias
  }

  def getBoardSize(board : Board): Int = {
    (board foldLeft 0)((r, x) => 1 + r) //usando fold pegariamos o tamanho da board
  }

  def filterOutCoord(lstOpenCoords : List[Coord2D], targetCoord : Coord2D) : List[Coord2D] = lstOpenCoords match { //recebe uma lista de coordenadas vazia e a coordenada a ser retirada
    case Nil => Nil //e da-nos a lista sem essa coordenada
    case head :: tail =>
      if(head == targetCoord)
        filterOutCoord(tail, targetCoord)
      else
        head :: filterOutCoord(tail, targetCoord)
  }

  //  T2
  def play(board : Board, player : Stone, coord : Coord2D, lstOpenCoords : List[Coord2D]) : (Option[Board], List[Coord2D]) = {//option
    if(!isCoordEmpty(coord, lstOpenCoords))//se nao estiver na lista de coordenadas vazias é porque já foi preenchida
      (None, lstOpenCoords) //não tem nada
    else
      val (row, column) = coord // se estiver pega a linha e coluna da coordenada
      val newBoard = board.updated(row, board(row).updated(column, player))   // atualiza a board com o novo elemento
      (Some(newBoard), filterOutCoord(lstOpenCoords, coord)) //tem algo
  }

  //  T3
  //  prof diz que podemos adicionar a posicao que foi jogada na tuple retornada.
  //  util para implementar 'Undo'
  def playRandomly(board:Board, r:MyRandom, player:Stone, lstOpenCoords:List[Coord2D],
                   f:(List[Coord2D], MyRandom) => (Coord2D,MyRandom)
                  ) : (Board,MyRandom,List[Coord2D]) =
  {
    val (newCoord2D, myRandomInstance) = f(lstOpenCoords, r) //vamos usar a funcao myrandom no main para obter a coordenada aleatoria e o new rand
    val (newBoardRes, updatedLstOpenCoords) = play(board, player, newCoord2D, lstOpenCoords) // dá-nos a board e lista de vazios atualizada
    newBoardRes match {
      case Some(newBoard) =>
        (newBoard, myRandomInstance, updatedLstOpenCoords)
      case None => (board, myRandomInstance, lstOpenCoords) //caso nao tenha nada devolve a board anterior
    }
  }

  def getBoardEmptyCoords(board: Board): List[Coord2D] = { //dá-nos as coordenadas vazias de uma board

    def getRowEmptyCoords(lst:List[Stone], currRow:Int, acc:Int) : List[Coord2D] = lst match{ //pega os zeros da linha
      case Nil ⇒ Nil
      case head :: tail ⇒
        if(head != Stone.Empty) getRowEmptyCoords(tail, currRow, acc + 1) //acc vai ser para sabermos a coluna
        else (currRow, acc) :: getRowEmptyCoords(tail, currRow, acc + 1)
    }

    def loopRowLists(board:Board, acc:Int) : List[Coord2D] = board match {  //faz isso com todas as linhas da board
      case Nil => Nil
      case head :: tail => getRowEmptyCoords(head, acc, 0) ++ loopRowLists(tail, acc + 1)
    }

    loopRowLists(board, 0) //começa na linha 0
  }

  //  T4
  @tailrec//garante que faça recursivamente e n faça overflow
  def drawBoard(board: Board): Unit = {//desenha a board

    @tailrec
    def drawLine(line: List[ Stone ]): Unit = line match { //desenha a linha
      case Nil => () //caso seja a linha vazia retorna ()
      case head :: tail =>
        head match {
          case Stone.Empty => print( " . " )
          case Stone.Black => print( " B " )
          case Stone.White => print( " W " )
        }
        drawLine( tail )  //desenha o resto da linha
    }

    board match {
      case Nil => ()
      case head :: tail =>
        drawLine( head )
        println()
        drawBoard( tail ) //desenha todas as linhas recursivamente
    }
  }

  def neighbors(board: Board, coord : Coord2D): List[Coord2D] = {
    val boardSize = getBoardSize(board)
    List(
      (coord._1 - 1, coord._2), // left
      (coord._1 + 1, coord._2), // right
      (coord._1, coord._2 - 1), // lower
      (coord._1, coord._2 + 1)  // upper
    ).filter((p, q) => p >= 0 && p < boardSize && q >= 0 && q < boardSize)
  }

  def getGroup(board : Board, startCoord : Coord2D) : List[Coord2D] = {
    @tailrec
    def getGroupAux(listAux : List[Coord2D], visited : List[Coord2D]) : List[Coord2D] = listAux match {
      case Nil => visited
      case head :: tail =>
        if(visited.contains(head))
          getGroupAux(tail, visited)
        else
          val currStoneNeighbors = neighbors(board, head)
          val x = currStoneNeighbors.filter((l, r) => board(l)(r).equals(board(head._1)(head._2)) && !visited.contains(head)) //????
          getGroupAux(tail ++ x, head :: visited)
    }
    getGroupAux(List(startCoord), List())
  }

  def isSurrounded(board: Board, group : List[Coord2D]) : Boolean = {
    def checkGroup(groupAux : List[Coord2D]) : Boolean = {
      (groupAux foldRight true)((x, r) => checkCoord(x) && r)
    }
    def checkCoord(coord: Coord2D): Boolean = {
      val list = neighbors(board, coord)
      (list foldRight true)((x, r) => !board(x._1)(x._2).equals(Stone.Empty) && r)
    }
    checkGroup(group)
  }

  def removeCapturedFromBoard(board: Board, group: List[Coord2D]): Board = {
    group.foldLeft(board) { case (b, Coord2D(row, col)) =>
      b.updated(row, b(row).updated(col, Stone.Empty))
    }
  }
  
  def removeStonesFromBoard(board:Board, coords:List[Coord2D]) : Board = {
    def iterateRows(currentboard:Board, coordsToRemove:List[ Coord2D ], acc_currRow:Int): Board = currentboard match {
      case Nil => Nil
      case row :: remainingRows => 
        val (newRow, remainingCoords) = removeFromRow (row, acc_currRow, coordsToRemove, 0)
        newRow :: iterateRows(remainingRows, remainingCoords, acc_currRow + 1)
    }
    
    def removeFromRow(rowList: List[ Stone ], currRowId:Int, coordsToRemove: List[ Coord2D ], acc_currColId:Int) : (List[Stone], List[Coord2D]) = rowList match {
      case Nil => (List(), coordsToRemove)
      case currStone :: tail =>
        if (coordsToRemove.isEmpty) (currStone :: tail, coordsToRemove)
        else
          if(coordsToRemove.head == (currRowId, acc_currColId) )
            (Stone.Empty :: removeFromRow(tail, currRowId, coordsToRemove.tail, acc_currColId + 1)._1, coordsToRemove.tail)
          else
            (currStone :: removeFromRow(tail, currRowId, coordsToRemove, acc_currColId + 1)._1, coordsToRemove)
    }
    iterateRows(board, coords, 0)
  }
  
// T5
  def captureGroupStones(board: Board, player: Stone): (Board, Int) = {
    val opponentStone = getOppositeStone(player)

    def findCapturedStones(currBoard: Board, remainingBoard:Board, opponent: Stone, acc_currRowId:Int = 0) : List[Coord2D] = remainingBoard match {
      case Nil => Nil
      case currentRow :: remainingRows =>
        findCapturedStonesInRow(currBoard, currentRow, opponent, acc_currRowId) ::: findCapturedStones(currBoard, remainingRows, opponent, acc_currRowId+1)
    }
    
    def findCapturedStonesInRow(currBoard: Board, rowList:List[Stone], opponent:Stone, currRowId: Int, acc_currColId: Int = 0) : List[Coord2D] = rowList match{
      case Nil => Nil
      case head :: tail =>
          if(head == opponent)
            if( isSurrounded(currBoard, getGroup(currBoard, (currRowId, acc_currColId) ) ) )
              (currRowId, acc_currColId) :: findCapturedStonesInRow(currBoard, tail, opponent, currRowId, acc_currColId + 1)
            else findCapturedStonesInRow(currBoard, tail, opponent, currRowId, acc_currColId + 1)
          else findCapturedStonesInRow(currBoard, tail, opponent, currRowId, acc_currColId + 1)
    }
    val capturedOpponentStonesList = findCapturedStones(board, board, opponentStone)
    println("Lista de capturadas: " + capturedOpponentStonesList)
    val amountCaptured = capturedOpponentStonesList.length
    (removeStonesFromBoard(board, capturedOpponentStonesList), amountCaptured)
  }

// T6
//  Refazer
  def play(board: Board, currPlayer: Stone, move: Coord2D, lstOpenCoords: List[Coord2D], playerTurnTimestamp:Int, playerTurnMaxTime: Int, scores:(Int, Int)) : Stone = isPlayerTurnTimeUp(playerTurnTimestamp, playerTurnMaxTime) match{
    case true => play(board, getOppositeStone(currPlayer), (-1,-1), lstOpenCoords, 0, playerTurnMaxTime, scores)
    case false => {
      // stuff
      Stone.Black
    }
  }
// T7
//  Tempo de jogada do jogador acabou?
  def isPlayerTurnTimeUp(playerTurnTimestamp:Int, playerTurnMaxTime:Int) : Boolean = {
    if( System.currentTimeMillis() > playerTurnTimestamp + playerTurnMaxTime) true else false
  }
  
  val r = MyRandom(System.currentTimeMillis())
  val s = GameState( 0, 0 )
  
  mainLoop( s, r )
  
  @tailrec
  def mainLoop(gameState: GameState, random: MyRandom) : Unit = {
    
    showPrompt()
    val userInput = getUserInput()
    
    // handle the result
    userInput match {
      case "H" | "T" => {
        val coinTossResult = tossCoin( random )
        val newNumFlips = gameState.numFlips + 1
        if( userInput == coinTossResult ) {
          val newNumCorrect = gameState.numCorrect + 1
          val newGameState = gameState.copy( numFlips = newNumFlips, numCorrect = newNumCorrect )
          printGameState( printableFlipResult( coinTossResult ), newGameState )
          mainLoop( newGameState, random )
        } else {
          val newGameState = gameState.copy( numFlips = newNumFlips )
          printGameState( printableFlipResult( coinTossResult ), newGameState )
          mainLoop( newGameState, random )
        }
      }
      
      case _ => {
        printGameOver()
        printGameState( gameState )
        // return out of the recursion here
      }
    }
  }
}
