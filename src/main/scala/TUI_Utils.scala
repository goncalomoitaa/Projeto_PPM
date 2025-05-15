import AtariGo.Stone._
import TUI_Utils.State.*

import scala.io.StdIn.readLine
import MyRandom.*

case class GameState(state:State,
                     currentTurnTimestamp:Long = 0,
                     currentTurn:Int = 0,
                     gameSize:Int = 5,
                     maxCap:Int = 1,
                     maxTurnTimeSec:Int = 60,
                     playerStone:Stone = Black,
                     playerCap:Int = 0,
                     opponentCap:Int = 0)

object TUI_Utils {
  
  object State extends Enumeration{
    type State = Value
    //    type Captured = Boolean
    val NONE,
        MENU,
        MENU_SET_GAME_SIZE,
        MENU_SET_CAP_LIMIT,
        MENU_SET_TURN_TIME_LIMIT,
        MENU_SET_PLAYER_STONE_COLOR,
        IN_GAME = Value
  }
  
  def showMenuPrompt(state:State): Unit = state match{
    case State.MENU =>
      println("Atari Go:")
      println("1: Comecar o jogo ")
      println("2: Tamanho do tabuleiro")
      println("3: Numero de capturas")
      println("4: Tempo de turno")
      println("5: Cor da pedra")
      println("6: Sair")
      
    case State.MENU_SET_GAME_SIZE => print("Define o tamanho da tabela: ")
    case State.MENU_SET_CAP_LIMIT => print("Define o numero maximo de capturas: ")
    case State.MENU_SET_TURN_TIME_LIMIT => print("Define o tempo limite de cada turno (segundos): ")
    case State.MENU_SET_PLAYER_STONE_COLOR =>
      println("Define a cor da pedra do jogador")
      print("(B)ranco ou (P)reto? ")
    case _ => ()
  }

  def getUserInput: String = readLine.trim.toUpperCase
  
  def handleMenuInput( menuState:GameState, userInput:String ) : GameState = menuState.state match{
    case State.MENU =>
      userInput match{
        case "1" => GameState(State.IN_GAME,
                              System.currentTimeMillis(),
                              1,
                              menuState.gameSize,
                              menuState.maxCap,
                              menuState.maxTurnTimeSec,
                              menuState.playerStone)        //  0, 0 é desnecessario, sao valores default
      }
    /*case State.MENU_SET_GAME_SIZE => print("Define o tamanho da tabela: ")
      userInput match{
      
      }
    case State.MENU_SET_CAP_LIMIT => print("Define o numero maximo de capturas: ")
      userInput match{
      
      }
    case State.MENU_SET_TURN_TIME_LIMIT => print("Define o tempo limite de cada turno: ")
      userInput match{
      
      }
    case State.MENU_SET_PLAYER_STONE_COLOR =>
      userInput match{
      
      }*/
  }
  
  def printGameState(gameState: GameState): Unit = {
    println(s"#Jogador: ${gameState.playerCap}, #Oponente: ${gameState.opponentCap}")
    println(s"#Limite: ${gameState.maxCap}")
  }

  def terminateGame(gameState : GameState) : Unit = 
  {
    println("\n=== GAME OVER ===")
  }
  
}
