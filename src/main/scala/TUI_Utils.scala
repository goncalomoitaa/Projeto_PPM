import AtariGo.Stone.*
import AtariGo.{Board, Coord2D, checkWinConditions, Timer}
import TUI_Utils.State as state
import TUI_Utils.State.*

import scala.io.StdIn.readLine

case class GameState(state:State,
                     turnTimer:Timer = Timer.start(),
                     currentTurn:Int = 0,
                     gameSize:Int = 5,
                     maxCap:Int = 1,
                     maxTurnTimeSec:Int = 5,
                     playerStone:Stone = Black,
                     playerCap:Int = 0,
                     opponentCap:Int = 0,
                     currentStone:Stone = Black,
                     board: Board = Nil,
                     oldState: GameState = null,
                     )

object TUI_Utils {
  
  object State extends Enumeration{
    type State = Value
    //    type Captured = Boolean
    val NONE,
    //  Menu IDs Section
        MENU,
        MENU_SET_GAME_SIZE,
        MENU_SET_CAP_LIMIT,
        MENU_SET_TURN_TIME_LIMIT,
        MENU_SET_PLAYER_STONE_COLOR,
    //  Gameplay States
        NEW_GAME,
        IN_GAME = Value
  }
  
  def stoneTranslate(stone:Stone) : String = stone match{
    case Black => "Preta"
    case White => "Branca"
    case _ => "Vazio"
  }
  
  def showMenuPrompt(gameState:GameState): Unit = gameState.state match{
    case State.MENU =>
        println("Atari Go:")
        println("1: Comecar o jogo ")
        println(s"2: Tamanho do tabuleiro <${gameState.gameSize}x${gameState.gameSize}>")
        println(s"3: Numero de capturas <${gameState.maxCap}>")
        println(s"4: Tempo de turno <${gameState.maxTurnTimeSec}s>")
        println(s"5: Cor da pedra <${stoneTranslate(gameState.playerStone)}>")
        println("6: Sair")
        print("Escolha uma opção: ")
      
    case State.MENU_SET_GAME_SIZE => print("Define o tamanho de tabuleiro [5-19]: ")
    case State.MENU_SET_CAP_LIMIT => print(s"Define o numero maximo de capturas [1-${gameState.gameSize}]: ")
    case State.MENU_SET_TURN_TIME_LIMIT => print("Define o tempo limite de cada turno em segundos [5-60]: ")
    case State.MENU_SET_PLAYER_STONE_COLOR =>
      println("Define a cor da pedra do jogador. (B)ranco ou (P)reto?: ")
    case _ => ()
  }

  def getUserInput: String = readLine.trim.toUpperCase

  def getInputCoord2D(s : String) : Coord2D = {
    val coord = s.trim.split(",")
    if(coord.length == 2)
        (coord(0).trim.toInt, coord(1).trim.toInt)
    else
        (-1, -1)    // invalid coord
      //throw new IllegalArgumentException("formato errado!!")
  }
  
  def handleMenuInput( menuState:GameState, userInput:String ) : GameState = menuState.state match{
    case State.MENU =>
      userInput match{
        case "1" => menuState.copy(state = State.NEW_GAME)
        case "2" => menuState.copy(state = State.MENU_SET_GAME_SIZE)
        case "3" => menuState.copy(state = State.MENU_SET_CAP_LIMIT)
        case "4" => menuState.copy(state = State.MENU_SET_TURN_TIME_LIMIT)
        case "5" => menuState.copy(state = State.MENU_SET_PLAYER_STONE_COLOR)
        case "6" | "Q" =>
            terminateGame(menuState)
      }
      
    case State.MENU_SET_GAME_SIZE =>
        userInput.toIntOption match {
            case Some(number) =>
              if( number < 5 || number > 19 ) {
                print( "Valor inválido. " )
                menuState
              }
              else {
                println(s"Definiu um novo tamanho de tabuleiro: $number")
                menuState.copy( state = State.MENU, gameSize = number)
              }
            case None =>
                print("Valor inválido. ")
                menuState
        }
      
      case State.MENU_SET_CAP_LIMIT => print("Define o numero maximo de capturas: ")
        userInput.toIntOption match {
          case Some( number ) =>
            if( number <= 0 || number > menuState.gameSize ) {
              print( "Valor inválido. " )
              menuState
            }
            else {
              println( s"Definiu um novo numero maximo de capturas: $number" )
              menuState.copy( state = State.MENU, maxCap = number )
            }
          case None =>
            print( "Valor inválido. " )
            menuState
        }
      case State.MENU_SET_TURN_TIME_LIMIT => print("Define o tempo limite de cada turno: ")
        userInput.toIntOption match {
          case Some( number ) =>
            if( number < 5 || number > 60 ) {
              print( "Valor inválido. " )
              menuState
            }
            else {
              println( s"Definiu um novo tempo limite por turno: $number" )
              menuState.copy( state = State.MENU, maxTurnTimeSec = number )
            }
          case None =>
            print( "Valor inválido. " )
            menuState
        }
    case State.MENU_SET_PLAYER_STONE_COLOR =>
      userInput match {
        case "B" => menuState.copy( state = State.MENU, playerStone = White )
        case "P" => menuState.copy( state = State.MENU, playerStone = Black )
        case _ => menuState
      }
  }
  
  def printGameState(gameState: GameState): Unit = {
    println(s"#Jogador: ${gameState.playerCap}, #Oponente: ${gameState.opponentCap} | #Limite: ${gameState.maxCap}")
    println(s"Turno nº${gameState.currentTurn}")
  }

  def terminateGame(gameState : GameState) : GameState = {
        val playerWon = checkWinConditions( gameState.maxCap, gameState.playerCap )
        
        if( playerWon )
            println( "\n=== VENCEU A PARTIDA ===\n" )
        else
            println( "\n=== PERDEU A PARTIDA ===\n" )
        gameState.copy( state = MENU )
  }

  def quitGame(gameState: GameState): GameState = {
    if (gameState.state == IN_GAME) {
      println("\n=== DESISTIU DA PARTIDA ===\n")
      gameState.copy(state = MENU)
    } else {
      println("\n=== OBRIGADO POR JOGAR, ATÉ À PROXIMA! ===")
      gameState.copy(state = NONE)
    }
  }
}
