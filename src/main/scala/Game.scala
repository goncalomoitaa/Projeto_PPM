import javafx.fxml.{FXML, Initializable}
import javafx.scene.control.Label
import javafx.scene.shape.{Circle, StrokeType}
import javafx.scene.paint.Color
import javafx.scene.input.MouseEvent
import javafx.scene.layout.GridPane
import java.util.ResourceBundle
import java.net.URL
import javafx.application.Platform
import javafx.geometry.{HPos, VPos}

import AtariGo.Stone.Stone
import AtariGo.{Board, Coord2D, Stone, Timer, captureGroupStones, checkWinConditions, getOppositeStone, playRandomly, randomMove}
import TUI_Utils.*

import scala.annotation.tailrec
import scala.language.postfixOps

class Game(gameState: GameState) extends Initializable{
    @FXML private var boardGrid : GridPane = _
    @FXML private var playerColor: Color = _
    @FXML private var botColor: Color = _
    @FXML private var playerScoreLabel: Label = _
    @FXML private var botScoreLabel: Label = _
    @FXML private var capLimitLabel: Label = _
    @FXML private var timerLabel: Label = _
    
    private var random: MyRandom = _
    private val funcRand = randomMove(_: List[Coord2D], _: MyRandom)
    private var currentGame: GameState = gameState

    @FXML
    override def initialize(url: URL, resourceBundle: ResourceBundle): Unit = {
        random = MyRandom(System.currentTimeMillis())
        playerColor = if (currentGame.playerStone == Stone.Black) Color.BLACK else Color.WHITE
        botColor = if (currentGame.playerStone == Stone.White) Color.BLACK else Color.WHITE
        
        currentGame = currentGame.copy(state = State.IN_GAME,
                                        turnTimer = Timer.start(),
                                        board = AtariGo.initializeBoard(currentGame.gameSize))
        
        playerScoreLabel.setText(s"PLAYER -> Score: ${currentGame.playerCap}")
        botScoreLabel.setText(s"BOT -> Score: ${currentGame.opponentCap}")
        capLimitLabel.setText(s"CAP LIMIT: ${currentGame.maxCap}")
        
        initNextTurn()
    }
    
    def initNextTurn() : Unit = {
        val nextTurn = currentGame.currentTurn + 1
        println(s"current turn: $nextTurn")
        if (nextTurn % 2 == 1)   // Odd number, black stone plays
        {
            currentGame = currentGame.copy(turnTimer = Timer.start(),currentTurn = nextTurn, currentStone = Stone.Black)
        }
        else
        {
            currentGame = currentGame.copy(turnTimer = Timer.start(),currentTurn = nextTurn, currentStone = Stone.White)
        }
        
        if( currentGame.currentStone != currentGame.playerStone ){
            // add a delay ?
            botPlay()
        }
        
        // is player turn, wait for input
    }
    
    @FXML
    def onGameBoardClick(event: MouseEvent): Unit = {
        if(currentGame.state != State.IN_GAME) return
        
        if(currentGame.currentStone != currentGame.playerStone) return
        
        val mouseX = event.getX
        val mouseY = event.getY

        val cols = boardGrid.getColumnConstraints.size()
        val rows = boardGrid.getRowConstraints.size()

        val cellWidth = boardGrid.getWidth / cols
        val cellHeight = boardGrid.getHeight / rows

        val col = (mouseX / cellWidth).toInt
        val row = (mouseY / cellHeight).toInt

        // Safety check for bounds
        if (col < 0 || col >= cols || row < 0 || row >= rows) return
            
        // Check if a circle is already in this cell
        val alreadyOccupied = boardGrid.getChildren.stream().anyMatch { node =>
            GridPane.getColumnIndex(node) == col &&
              GridPane.getRowIndex(node) == row &&
              node.isInstanceOf[Circle]
        }
        if (alreadyOccupied) {
            return
        }
        
        val validPlay = playerPlay(col, row)
        if (!validPlay)
            return
            
        println(s"Processed valid Play $row $col, state ${currentGame.state}, turn ${currentGame.currentTurn}")
    }

    private def playerPlay(col: Int, row: Int): Boolean = {
        val board = currentGame.board
        val lstOpenCoords = AtariGo.getBoardEmptyCoords(board)
        val (newBoardRes, newLstOpenCoords) = AtariGo.play(board, currentGame.playerStone, (row, col), lstOpenCoords)
        newBoardRes match {
            case Some(boardAfter) =>
                //val newBoard = getBoard(newBoardRes, board)
                //updateGameBoard(boardAfter)
                //val circle = createCircle(playerColor)
                drawPosition(playerColor, col, row)
                
                val (newBoardAfterPlay, capturesAmount, won) = removeCaptures(boardAfter, currentGame.currentStone)
                if (won) {
                    println("You won!")
                    currentGame = currentGame.copy(state = State.NONE)
                    Platform.exit()
                    return true
                }
                updateGameState(newBoardAfterPlay, capturesAmount)
                initNextTurn()
                true
            case None => false
        }
    }

    @tailrec
    private def botPlay(): Unit = {
        val oldBoard = currentGame.board
        val lstOpenCoords = AtariGo.getBoardEmptyCoords(oldBoard)
        val (newBoard, newRandom, newLstOpenCoords) = playRandomly(oldBoard, random, AtariGo.getOppositeStone(currentGame.playerStone), lstOpenCoords, funcRand)
        // playRandomly already returns a resolved "Some" object
        // it returns either a new board or the old one. Must check which one
        
        if(oldBoard != newBoard)   // It is a new one
        {
            //updateGameBoard(newBoard)
            random = newRandom
            
            val playedPosList = lstOpenCoords.filter(coord => !newLstOpenCoords.contains(coord))
            if (playedPosList.isEmpty)
                println("WOULD CRASH HERE")
            else{
                val playedPos = playedPosList.head //lstOpenCoords.filter(coord => !newLstOpenCoords.contains(coord)).head
                drawPosition(botColor, playedPos._2, playedPos._1)
            }
            
            //val circle = createCircle(botColor)
            
            val (newBoardAfterPlay, capturesAmount, won) = removeCaptures(newBoard, currentGame.currentStone)
            if (won) {
                println("You Lost!")
                currentGame = currentGame.copy(state = State.NONE)
                Platform.exit()
                return
            }
            updateGameState(newBoardAfterPlay, capturesAmount)
            initNextTurn()
        }
        else {
            println("Something went wrong, must try again")
            AtariGo.drawBoard(oldBoard)
            println(s"Old list: $lstOpenCoords")
            println(s"new list: $newLstOpenCoords")
            random = newRandom
            botPlay()
        }
    }

    private def removeCaptures(board:Board, stone: Stone): (Board, Int, Boolean) = {
        //println(s"removeCaptures($stone)")
        val lstOpenCoords = AtariGo.getBoardEmptyCoords(board)
        val (newBoardAfterPlay, capturesAmount, won) = checkForCapturesAndWins(board, stone)
        //updateCaptures(stone, capturesAmount)
        if (capturesAmount > 0) {
            val lstCoordsToRemoveStonesFrom = AtariGo.getBoardEmptyCoords(newBoardAfterPlay).filter(c => !lstOpenCoords.contains(c))
            for (coord <- lstCoordsToRemoveStonesFrom) {
                println(s"Removing stone at row ${coord._1},col ${coord._2}")
                removeCircleAt(boardGrid, coord._2, coord._1)
            }
        }
        (newBoardAfterPlay, capturesAmount, won)
    }

    private def getBoard(board: Option[Board], oldBoard: Board): Board = {
        board match {
            case Some(b) => b
            case None => oldBoard
        }
    }

    def removeCircleAt(grid: GridPane, col: Int, row: Int): Unit = {
        val nodeToRemove = grid.getChildren.stream()
          .filter(node =>
              GridPane.getColumnIndex(node) == col &&
                GridPane.getRowIndex(node) == row &&
                node.isInstanceOf[Circle]
          )
          .findFirst()

        if (nodeToRemove.isPresent) {
            grid.getChildren.remove(nodeToRemove.get())
        }
    }

    def updateGameBoard(board: Board): Unit = {
        val newGameState = currentGame.copy(
            board = board
        )
        currentGame = newGameState
    }

    private def updateGameState(boardRes: Board, capturesAmount:Int): Unit = {
        if (currentGame.currentStone == currentGame.playerStone){
            val newGameState = currentGame.copy(
                board = boardRes,
                currentStone = getOppositeStone(currentGame.playerStone),
                playerCap = currentGame.playerCap + capturesAmount,
                oldState = currentGame
            )
            currentGame = newGameState
            updateCapturesLabel(currentGame.playerStone)
        } else {
            val newGameState = currentGame.copy(
                board = boardRes,
                currentStone = currentGame.playerStone,
                opponentCap = currentGame.opponentCap + capturesAmount,
            )
            currentGame = newGameState
            updateCapturesLabel(getOppositeStone(currentGame.playerStone))
        }
        AtariGo.drawBoard(currentGame.board)
    }

    private def updateCapturesLabel(currentPlayer: Stone): Unit = {
        if (currentGame.playerStone == currentPlayer) {
            //println(s"updatingCaptures: PLAYER -> Score: ${currentGame.playerCap}")
            playerScoreLabel.setText(s"PLAYER -> Score: ${currentGame.playerCap}")
            //val addCaptures = capturesAmount + currentGame.playerCap
            //println(s"addCaptures = capturesAmount + currentGame.playerCap => $addCaptures=$capturesAmount+${currentGame.playerCap}")
            //currentGame = currentGame.copy(playerCap = addCaptures)
        } else {
            //println(s"updatingCaptures: BOT -> Score: ${currentGame.opponentCap}")
            botScoreLabel.setText(s"BOT -> Score: ${currentGame.opponentCap}")
            //val addCaptures = currentGame.opponentCap + capturesAmount
            //println(s"addCaptures = capturesAmount + currentGame.opponentCap => $addCaptures=$capturesAmount+${currentGame.opponentCap}")
            //currentGame = currentGame.copy(opponentCap = addCaptures)
        }
    }

    private def getCurrentPlayer(color: Color): Stone = color match {
        case s if s == playerColor => currentGame.playerStone
        case s if s == botColor => getOppositeStone(currentGame.playerStone)
        case _ => Stone.Empty
    }

    private def checkForCapturesAndWins(board:Board, currentStone:Stone): (Board, Int, Boolean) = {
        val (boardAfterPlay, capturesAmount) = captureGroupStones(board, currentStone)

        if( currentGame.playerStone == currentStone ) {
            //currentGame = currentGame.copy(board = boardAfterPlay, playerCap = currentGame.playerCap + capturesAmount)
            val winner = checkWinConditions(currentGame.maxCap, currentGame.playerCap + capturesAmount)
            (boardAfterPlay, capturesAmount, winner)

        } else {
            //currentGame = currentGame.copy(board = boardAfterPlay, opponentCap = currentGame.opponentCap + capturesAmount)
            //val winner = checkWinConditions(currentGame.maxCap, currentGame.opponentCap)
            val winner = checkWinConditions(currentGame.maxCap, currentGame.opponentCap + capturesAmount)
            (boardAfterPlay, capturesAmount, winner)
            (boardAfterPlay, capturesAmount, winner)
        }

    }

    private def createCircle(color: Color): Circle = {
        val circle = new Circle(35)
        circle.setFill(color)
        circle.setStrokeType(StrokeType.INSIDE)
        circle.setStroke(Color.BLACK)
        circle.setStrokeWidth(1)
        circle.setOpacity(1.0)

        circle
    }

    private def drawPosition(player: Color, col: Int, row: Int): Unit = {
        val circle = createCircle(player)
        GridPane.setColumnIndex(circle, col)
        GridPane.setRowIndex(circle, row)
        GridPane.setHalignment(circle, HPos.CENTER)
        GridPane.setValignment(circle, VPos.CENTER)
        boardGrid.getChildren.add(circle)
    }
}
