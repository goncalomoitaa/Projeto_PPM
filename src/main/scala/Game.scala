import AtariGo.Stone.Stone
import javafx.fxml.{FXML, Initializable}
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.shape.{Circle, StrokeType}
import javafx.scene.input.{MouseEvent, TouchEvent}
import javafx.scene.layout.GridPane
import javafx.scene.paint.Color
import javafx.scene.input.MouseEvent
import javafx.scene.layout.GridPane

import java.net.URL
import java.util.ResourceBundle
import TUI_Utils.*
import AtariGo.{Board, Coord2D, Stone, Timer, captureGroupStones, checkWinConditions, getOppositeStone, playRandomly, randomMove}
import javafx.application.Platform
import javafx.geometry.{HPos, VPos}

import scala.language.postfixOps

class Game(gameState: GameState) extends Initializable{
    @FXML
    private var boardGrid : GridPane = _
    @FXML
    private var playerColor: Color = _
    @FXML
    private var botColor: Color = _

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
        println("onGameBoardClick")
        println(s"state ${currentGame.state}, currentStone ${currentGame.currentStone}")
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
        
        playerPlay(col, row)
        
        // Check if a circle is already in this cell
        val alreadyOccupied = boardGrid.getChildren.stream().anyMatch { node =>
            GridPane.getColumnIndex(node) == col &&
              GridPane.getRowIndex(node) == row &&
              node.isInstanceOf[Circle]
        }
        if (alreadyOccupied) {
            return
        }
    }

    private def playerPlay(col: Int, row: Int): Unit = {
        val board = currentGame.board
        val lstOpenCoords = AtariGo.getBoardEmptyCoords(board)
        val (newBoardRes, newLstOpenCoords) = AtariGo.play(board, currentGame.playerStone, (row, col), lstOpenCoords)
        newBoardRes match {
            case Some(boardAfter) =>
                val newBoard = getBoard(newBoardRes, board)
                updateGameBoard(newBoard)
                val circle = createCircle(playerColor)
                drawPosition(playerColor, col, row)
                
                val (newBoardAfterPlay, won) = removeCaptures(currentGame.currentStone)
                if (won) {
                    println("You won!")
                    currentGame = currentGame.copy(state = State.NONE)
                    Platform.exit()
                    return
                }
                updateGameState(newBoardAfterPlay, playerColor)
                initNextTurn()
            
            case None => ()
        }
    }

    private def botPlay(): Unit = {
        val board = currentGame.board
        val lstOpenCoords = AtariGo.getBoardEmptyCoords(board)
        val (newBoard, newRandom, newLstOpenCoords) = playRandomly(board, random, AtariGo.getOppositeStone(currentGame.playerStone), lstOpenCoords, funcRand)
        newBoard match {
            case boardAfter =>
                updateGameBoard(newBoard)
                random = newRandom
                val playedPos = lstOpenCoords.filter(coord => !newLstOpenCoords.contains(coord)).head
                val circle = createCircle(botColor)
                drawPosition(botColor, playedPos._2, playedPos._1)
                
                val (newBoardAfterPlay, won) = removeCaptures(currentGame.currentStone)
                if (won) {
                    println("You Lost!")
                    currentGame = currentGame.copy(state = State.NONE)
                    Platform.exit()
                    return
                }
                updateGameState(newBoardAfterPlay, botColor)
                initNextTurn()
            case Nil =>
                println("Something went wrong")
        }
    }

    private def removeCaptures(stone: Stone): (Board, Boolean) = {
        println(s"removeCaptures($stone)")
        val board = currentGame.board
        val lstOpenCoords = AtariGo.getBoardEmptyCoords(board)
        val (newBoardAfterPlay, capturesAmount, won) = checkForCapturesAndWins(board, stone)
        updateCaptures(stone, capturesAmount)
        if (capturesAmount > 0) {
            val lstCoordsToRemoveStonesFrom = AtariGo.getBoardEmptyCoords(newBoardAfterPlay).filter(c => !lstOpenCoords.contains(c))
            for (coord <- lstCoordsToRemoveStonesFrom) {
                println(s"Removing stone at row ${coord._1},col ${coord._2}")
                removeCircleAt(boardGrid, coord._2, coord._1)
            }
        }
        (newBoardAfterPlay, won)
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

    private def updateGameState(boardRes: Board, currentPlayer: Color): Unit = {
        if (currentPlayer == playerColor){
            val newGameState = currentGame.copy(
                board = boardRes,
                currentStone = getOppositeStone(currentGame.playerStone),
                oldState = currentGame
            )
            currentGame = newGameState
        } else {
            val newGameState = currentGame.copy(
                board = boardRes,
                currentStone = currentGame.playerStone
            )
            currentGame = newGameState
        }
    }

    private def updateCaptures(currentPlayer: Stone, capturesAmount: Int): Unit = {
        if (currentGame.playerStone == currentPlayer) {
            val addCaptures = capturesAmount + currentGame.playerCap
            currentGame = currentGame.copy(playerCap = addCaptures)
        } else {
            val addCaptures = currentGame.opponentCap + capturesAmount
            currentGame = currentGame.copy(opponentCap = addCaptures)
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
            currentGame = currentGame.copy(board = boardAfterPlay, playerCap = currentGame.playerCap + capturesAmount)
            val winner = checkWinConditions(currentGame.maxCap, currentGame.playerCap)
            (boardAfterPlay, capturesAmount, winner)

        } else {
            currentGame = currentGame.copy(board = boardAfterPlay, opponentCap = currentGame.opponentCap + capturesAmount)
            val winner = checkWinConditions(currentGame.maxCap, currentGame.opponentCap)
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
