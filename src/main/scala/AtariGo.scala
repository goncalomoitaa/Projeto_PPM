import AtariGo.Stone.Stone
import TUI_Utils.*

import scala.annotation.tailrec
import scala.collection.immutable.:: // {getUserInput, printGameOver, printGameState, printableFlipResult, showMenuPrompt, tossCoin}

import javafx.application.Application

object AtariGo{

  type Board = List[List[Stone]]
  type Coord2D = (Int, Int)       //(row, column)
  
  val INVALID_COORD : Coord2D = (-1,-1)
  
  object Stone extends Enumeration {
    type Stone = Value
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
  
  def play(board : Board, player : Stone, coord : Coord2D, lstOpenCoords : List[Coord2D]) : (Option[Board], List[Coord2D]) = {//option
    if(!isCoordEmpty(coord, lstOpenCoords) || isCoordSurrounded(coord, board, player))//se nao estiver na lista de coordenadas vazias é porque já foi preenchida
      (None, lstOpenCoords) //não tem nada
    else
      val (row, column) = coord // se estiver pega a linha e coluna da coordenada
      val newBoard = board.updated(row, board(row).updated(column, player))   // atualiza a board com o novo elemento
      (Some(newBoard), filterOutCoord(lstOpenCoords, coord)) //tem algo
  }
  
  def playRandomly(board:Board, r:MyRandom, player:Stone, lstOpenCoords:List[Coord2D],
                   f:(List[Coord2D], MyRandom) => (Coord2D,MyRandom)
                  ) : (Board,MyRandom,List[Coord2D]) = { //vamos usar a funcao myrandom no main para obter a coordenada aleatoria e o new rand
    
    @tailrec
    def getFreeCoord(random: MyRandom, coord2D: (Int, Int) = null): (Coord2D, MyRandom) = {
      if (coord2D == null) {
        val (newCoord, newRand) = f(lstOpenCoords, random) //obtemos a coordenada aleatória e o new rand
        if (isCoordEmpty(newCoord, lstOpenCoords)) (newCoord, newRand)
        else getFreeCoord( newRand, null ) //se a coordenada não estiver vazia chamamos novamente a funcao
      }
      else (coord2D, random) //se a coordenada for diferente de null devolvemos a coordenada e o rand
    }
    
    val (newCoord2D, myRandomInstance) = getFreeCoord( r, null ) //obtemos a coordenada aleatória e o new rand
    
    val (newBoardRes, updatedLstOpenCoords) = play(board, player, newCoord2D, lstOpenCoords) // dá-nos a board e lista de vazios atualizada
    newBoardRes match {
      case Some(newBoard) =>
        (newBoard, myRandomInstance, updatedLstOpenCoords)
      case None => (board, myRandomInstance, lstOpenCoords) //caso nao tenha nada devolve a board anterior
    }
  }

    def getBoardEmptyCoords(board: Board): List[Coord2D] = { //dá-nos as coordenadas vazias de uma board

      def getRowEmptyCoords(lst: List[Stone], currRow: Int, acc: Int): List[Coord2D] = lst match { //pega os zeros da linha
        case Nil ⇒ Nil
        case head :: tail ⇒
          if (head != Stone.Empty) getRowEmptyCoords(tail, currRow, acc + 1) //acc vai ser para sabermos a coluna
          else (currRow, acc) :: getRowEmptyCoords(tail, currRow, acc + 1)
      }

      def loopRowLists(board: Board, acc: Int): List[Coord2D] = board match { //faz isso com todas as linhas da board
        case Nil => Nil
        case head :: tail => getRowEmptyCoords(head, acc, 0) ++ loopRowLists(tail, acc + 1)
      }

      loopRowLists(board, 0) //começa na linha 0
    }
  
    @tailrec //garante que faça recursivamente e n faça overflow
    def drawBoard(board: Board): Unit = { //desenha a board

      @tailrec
      def drawLine(line: List[Stone]): Unit = line match { //desenha a linha
        case Nil => () //caso seja a linha vazia retorna ()
        case head :: tail =>
          head match {
//            case Stone.Empty => print(" . ")
//            case Stone.Black => print(" B ")
//            case Stone.White => print(" W ")
            //  Fancy ANSI Color escape codes
            case Stone.Empty => print("\u001b[0m . ")
            case Stone.Black => print("\u001b[40m . \u001b[0m")
            case Stone.White => print("\u001b[48;2;245;245;245m . \u001b[0m")
          }
          drawLine(tail) //desenha o resto da linha
      }

      board match {
        case Nil => ()
        case head :: tail =>
          drawLine(head)
          println()
          drawBoard(tail) //desenha todas as linhas recursivamente
      }
    }

    def neighbors(boardSize: Int, coord: Coord2D): List[Coord2D] = {
      List(
        (coord._1 - 1, coord._2), // left
        (coord._1 + 1, coord._2), // right
        (coord._1, coord._2 - 1), // lower
        (coord._1, coord._2 + 1) // upper
      ).filter((p, q) => p >= 0 && p < boardSize && q >= 0 && q < boardSize)
    }

    def getGroup(board: Board, startCoord: Coord2D): List[Coord2D] = {
      @tailrec
      def getGroupAux(listAux: List[Coord2D], visited: List[Coord2D]): List[Coord2D] = listAux match {
        case Nil => visited
        case head :: tail =>
          if (visited.contains(head))
            getGroupAux(tail, visited)
          else
            val currStoneNeighbors = neighbors(getBoardSize(board), head)
            val x = currStoneNeighbors.filter((l, r) => board(l)(r).equals(board(head._1)(head._2)) && !visited.contains(head)) //????
            getGroupAux(tail ++ x, head :: visited)
      }

      getGroupAux(List(startCoord), List())
    }

    def undo(gameState: GameState): GameState = {
      val oldState = gameState.oldState.copy(turnTimer = Timer.start())
      if (oldState != null) oldState
      else gameState
    }

    def isCoordSurrounded(coord: Coord2D, board: Board, player:Stone): Boolean = {
      if (coord == null) true
      else {
        //val list = neighbors(getBoardSize(board), coord)
        //(list foldRight true)((x, r) => !board(x._1)(x._2).equals(Stone.Empty) && !board( x._1 )( x._2 ).equals( player ) && r)
        val currCoordGroup = getGroup(board, coord)
        if (isGroupSurrounded(board, player, currCoordGroup)) true else false
      }
    }
    
    def isGroupSurrounded(board: Board, player:Stone, group: List[Coord2D]): Boolean = {
      def checkCoord(coord: Coord2D, board: Board, player: Stone): Boolean = {
        val list = neighbors(getBoardSize(board), coord)
        (list foldRight true)((x, r) => !board(x._1)(x._2).equals(Stone.Empty) /*&& !board( x._1 )( x._2 ).equals( player )*/ && r)
      }

      def checkGroup(groupAux: List[Coord2D]): Boolean = {
        (groupAux foldRight true)((x, r) => checkCoord(x, board, player) && r)
      }

      checkGroup(group)
    }

    def removeStonesFromBoard(board: Board, coords: List[Coord2D]): Board = {
      def iterateRows(currentboard: Board, coordsToRemove: List[Coord2D], acc_currRow: Int): Board = currentboard match {
        case Nil => Nil
        case row :: remainingRows =>
          val (newRow, remainingCoords) = removeFromRow(row, acc_currRow, coordsToRemove, 0)
          newRow :: iterateRows(remainingRows, remainingCoords, acc_currRow + 1)
      }

      def removeFromRow(rowList: List[Stone], currRowId: Int, coordsToRemove: List[Coord2D], acc_currColId: Int): (List[Stone], List[Coord2D]) = rowList match {
        case Nil => (List(), coordsToRemove)
        case currStone :: tail =>
          if (coordsToRemove.isEmpty) (currStone :: tail, coordsToRemove)
          else if (coordsToRemove.head == (currRowId, acc_currColId))
            (Stone.Empty :: removeFromRow(tail, currRowId, coordsToRemove.tail, acc_currColId + 1)._1, coordsToRemove.tail)
          else
            (currStone :: removeFromRow(tail, currRowId, coordsToRemove, acc_currColId + 1)._1, coordsToRemove)
      }

      iterateRows(board, coords, 0)
    }
  
    def getCurrentStoneScore(gameState: GameState): Int = {
        if( gameState.currentStone == gameState.playerStone ) gameState.playerCap
        else gameState.opponentCap
    }
  
    def captureGroupStones(board: Board, player: Stone): (Board, Int) = {
      val opponentStone = getOppositeStone(player)

      def findCapturedStones(currBoard: Board, remainingBoard: Board, opponent: Stone, acc_currRowId: Int = 0): List[Coord2D] = remainingBoard match {
        case Nil => Nil
        case currentRow :: remainingRows =>
          findCapturedStonesInRow(currBoard, currentRow, opponent, acc_currRowId) ::: findCapturedStones(currBoard, remainingRows, opponent, acc_currRowId + 1)
      }

      def findCapturedStonesInRow(currBoard: Board, rowList: List[Stone], opponent: Stone, currRowId: Int, acc_currColId: Int = 0): List[Coord2D] = rowList match {
        case Nil => Nil
        case head :: tail =>
          if (head == opponent)
            if (isGroupSurrounded(currBoard, opponent, getGroup(currBoard, (currRowId, acc_currColId))))
              (currRowId, acc_currColId) :: findCapturedStonesInRow(currBoard, tail, opponent, currRowId, acc_currColId + 1)
            else findCapturedStonesInRow(currBoard, tail, opponent, currRowId, acc_currColId + 1)
          else findCapturedStonesInRow(currBoard, tail, opponent, currRowId, acc_currColId + 1)
      }

      val capturedOpponentStonesList = findCapturedStones(board, board, opponentStone)
      //    println("Lista de capturadas: " + capturedOpponentStonesList)
      val amountCaptured = capturedOpponentStonesList.length
      (removeStonesFromBoard(board, capturedOpponentStonesList), amountCaptured)
    }
  
    def checkWinConditions(capLimit: Int, currentStoneScore: Int): Boolean = {
      // Retorna a cor da pedra que ganhou, None se ainda nao acabou o jogo
      if ( currentStoneScore >= capLimit)
        true
      else
        false
    }

  class Timer private(start: Long) {
    private def getMillisElapsed: Long = System.currentTimeMillis() - start
    def getSecondsElapsed: Int = (getMillisElapsed / 1000).toInt
  }

  object Timer {
    def start(): Timer = new Timer(System.currentTimeMillis())
  }

  def isPlayerTurnTimeUp(playerTurnTimestamp: Int, playerTurnMaxTime: Int): Boolean = {
    if (playerTurnTimestamp > playerTurnMaxTime) true else false
  }

  val funcRandMovePlay = randomMove(_: List[Coord2D], _: MyRandom)

  @tailrec
  def mainLoop(gameState: GameState, random: MyRandom): Unit = gameState.state match {

    case State.NEW_GAME =>
      val newState = gameState.copy(state = State.IN_GAME,
        currentStone = Stone.Black, currentTurn = 1, playerCap = 0, opponentCap = 0, board = initializeBoard(gameState.gameSize), turnTimer = Timer.start())

      mainLoop(newState, random)


    case State.IN_GAME =>
      print("\u001b[2J") // Clear screen
      print("\u001b[H") // Move cursor to top-left
      println("current Score: " + getCurrentStoneScore(gameState))
      val currGameState = checkWinConditions(gameState.maxCap, getCurrentStoneScore(gameState))
      println(gameState.currentStone)
      printGameState(gameState)
      drawBoard(gameState.board)
      if (currGameState) { //game reached a victory condition
        val newState = terminateGame(gameState)
        mainLoop(newState, random)
      }
      else // is in progress
      {
        //println("main game loop, currently ingame.")
        if (gameState.currentStone == gameState.playerStone) {
          val input = getUserInput
          val coord2D = getInputCoord2D(input)
          val (newBoard, _) = play(gameState.board, gameState.playerStone, coord2D, getBoardEmptyCoords(gameState.board))
          newBoard match {
            case Some(boardAfter) =>
              val (boardAfterPlay, capturesAmount) = captureGroupStones(boardAfter, gameState.playerStone)
              if (!isPlayerTurnTimeUp(gameState.turnTimer.getSecondsElapsed, gameState.maxTurnTimeSec)) {
                val newState = gameState.copy(oldState = gameState, playerCap = gameState.playerCap + capturesAmount, currentStone = getOppositeStone(gameState.playerStone), currentTurn = gameState.currentTurn + 1, board = boardAfterPlay)
                println(gameState.turnTimer.getSecondsElapsed)
                mainLoop(newState, random)
              }
              else {
                val newState = gameState.copy(oldState = gameState, playerCap = gameState.playerCap + capturesAmount, currentStone = getOppositeStone(gameState.playerStone), currentTurn = gameState.currentTurn + 1)
                println("\u001b[31mTempo de turno esgotado!\u001b[0m")
                println(gameState.turnTimer.getSecondsElapsed)
                mainLoop(newState, random)
              }
            case None => //  It was not a valid coord, check if it was a Quit attempt or just an invalid play
              if (input == "Q") {
                val newState = quitGame(gameState)
                mainLoop(newState, random)
              }
              else if (input == "U") {
                val newState = undo(gameState)
                mainLoop(newState, random)
              }
              else if (input == "R") {
                val newState = gameState.copy(state = State.MENU)
                mainLoop(newState, random)
              }
              else {
                println("Jogada inválida, tenta outra vez")
                mainLoop(gameState, random)
              }
          }
        }
        else { // CPU Player turn
          val (newBoard, r, _) = playRandomly(gameState.board, random, getOppositeStone(gameState.playerStone), getBoardEmptyCoords(gameState.board), funcRandMovePlay)
          newBoard match {
            case boardAfter =>
              val (boardAfterPlay, capturesAmount) = captureGroupStones(boardAfter, getOppositeStone(gameState.playerStone))
              val newState = gameState.copy(opponentCap = gameState.opponentCap + capturesAmount, currentStone = gameState.playerStone, currentTurn = gameState.currentTurn + 1, board = boardAfterPlay, turnTimer = Timer.start())
              mainLoop(newState, r)
            case Nil =>
              mainLoop(gameState, r)
          }
        }
      }

    case menu_id if menu_id >= State.MENU && menu_id < State.NEW_GAME => //  Menu IDs Range
      showMenuPrompt(gameState)
      val userInput = getUserInput
      val newState = handleMenuInput(gameState, userInput)
      mainLoop(newState, random)

    case _ => () //  Any other state, shuts down game (i.e. State.NONE)
  }

  def main(args: Array[String]): Unit = {
    if (args.length == 1 && args(0) == "-gui") {
      println("GUI mode enabled.")
      Application.launch(classOf[GUI], args: _*)
    } else {
      println("TUI mode enabled.")
      mainLoop(GameState(State.MENU), MyRandom(System.currentTimeMillis()))
    }
  }
}
