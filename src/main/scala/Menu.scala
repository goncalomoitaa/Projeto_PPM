import AtariGo.Stone
import javafx.fxml.{FXML, Initializable}
import javafx.scene.control.{Button, RadioButton, TextField, ToggleGroup}
import javafx.scene.paint.Color
import javafx.scene.layout.GridPane
import javafx.fxml.FXMLLoader
import javafx.application.Platform

import java.net.URL
import java.util.ResourceBundle
import AtariGo.Stone.Stone
import TUI_Utils.State
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

// Default Settings
val MIN_CAPS : Int = 1
val MAX_CAPS : Int = 10
val DEFAULT_CAPS: Int = MIN_CAPS
val MIN_TURNTIME : Int = 5
val MAX_TURNTIME : Int = 60
val DEFAULT_TURNTIME : Int = 30

class Menu extends Initializable {
    @FXML private var boardGrid : GridPane = _
    @FXML private var playerColor: Color = _
    @FXML private var botColor: Color = _
    @FXML private var gameState: GameState = _
    @FXML private var maxCapturesField:TextField = _
    @FXML private var turnTimeField:TextField = _
    @FXML private var teamGroup:ToggleGroup = _
    @FXML private var whiteRadio:RadioButton = _
    @FXML private var blackRadio:RadioButton = _
    @FXML private var startButton:Button = _
    
    private var gameStage: Option[ Stage ] = None
    
    @FXML
    override def initialize(url: URL, resourceBundle: ResourceBundle): Unit = {

        maxCapturesField.textProperty().addListener { (_, oldValue, newValue) =>
            if( !newValue.matches( "\\d*" ) ) {
                maxCapturesField.setText( oldValue ) // revert to old value if non-digit entered
            }else if (newValue.nonEmpty) {
                val value = newValue.toInt
                if( value < MIN_CAPS || value > MAX_CAPS ) {
                    println( s"$value is out of range! Resetting." )
                    Platform.runLater( () => maxCapturesField.setText( s"$DEFAULT_CAPS" ) ) // Safe reset
                }
            }
        }

        turnTimeField.textProperty().addListener { (_, oldValue, newValue) =>
            if( !newValue.matches( "\\d*" ) ) {
                turnTimeField.setText( oldValue ) // revert to old value if non-digit entered
            } else if( newValue.nonEmpty ) {
                val value = newValue.toInt
                if( value < MIN_TURNTIME || value > MAX_TURNTIME ) {
                    println( s"$value is out of range! Resetting." )
                    Platform.runLater( () => turnTimeField.setText( s"$DEFAULT_TURNTIME" ) ) // Safe reset
                }
            }
        }

        // Stone Color RadioButton selection listener
        teamGroup.selectedToggleProperty().addListener { (_, _, newToggle) =>
            if( newToggle != null ) {
                val selectedRadio = newToggle.asInstanceOf[ RadioButton ]
                //println( s"Selected team: ${selectedRadio.getId}" )
            } else {
                println( "No team selected" )
            }
        }
    }
    
    @FXML
    def onStartGame(): Unit = {
        gameStage match {
            case Some( stage ) if stage.isShowing =>
                stage.toFront()
            case _ =>
                val maxCaptures = maxCapturesField.getCharacters.toString.toInt
                val turnTime = turnTimeField.getCharacters.toString.toInt
                val gameState = GameState( State.NEW_GAME, maxTurnTimeSec = turnTime, maxCap = maxCaptures, playerStone = getSelectedStoneColor )
                val fxmlLoader = new FXMLLoader( getClass.getResource( "Game.fxml" ) )
                val gameController = new Game( gameState )
                fxmlLoader.setController( gameController )
                val mainViewRoot: Parent = fxmlLoader.load()
                val stage = new Stage()
                val scene = new Scene( mainViewRoot )
                gameStage = Some(stage)
                stage.setOnCloseRequest(_ => gameStage = None)
                stage.setTitle( "Atari Go" )
                stage.setScene( scene )
                stage.show()
        }
    }
    
    private def getSelectedStoneColor: Stone = {
        val selectedToggle = teamGroup.getSelectedToggle
        if( selectedToggle != null ) {
            val selectedRadio = selectedToggle.asInstanceOf[ RadioButton ]
            if( selectedRadio == blackRadio ) Stone.Black else Stone.White
        } else Stone.Black  // "No radio button selected", default to Black Stone
    }
    
    @FXML def onTurnTimeChange(): Unit = {
        val fieldInput = turnTimeField.getCharacters
    }
}
