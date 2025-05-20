import AtariGo.Stone
import javafx.fxml.{ FXML, Initializable }
import javafx.scene.control.{ Button, Label, RadioButton, TextField, ToggleGroup }
import javafx.scene.shape.{ Circle, StrokeType }
import javafx.scene.input.{ MouseEvent, TouchEvent }
import javafx.scene.layout.GridPane
import javafx.scene.paint.Color
import javafx.scene.input.MouseEvent
import javafx.scene.layout.GridPane

import java.net.URL
import java.util.ResourceBundle
import AtariGo.Stone.Stone
import TUI_Utils.{ State, printGameState }

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
    
    @FXML
    override def initialize(url: URL, resourceBundle: ResourceBundle): Unit = {
        
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
    def onStartGame(): GameState = {
        println("Button On Action triggered: game started!")
        
        val selectedColor = getSelectedStoneColor()
        GameState(State.NEW_GAME, )
    }
    
    def getSelectedStoneColor(): Stone = {
        val selectedToggle = teamGroup.getSelectedToggle
        if( selectedToggle != null ) {
            val selectedRadio = selectedToggle.asInstanceOf[ RadioButton ]
            if( selectedRadio == blackRadio ) Stone.Black else Stone.White
        } else Stone.Black  // "No radio button selected", default to Black Stone
    }
    
    @FXML def onTurnTimeChange() = Unit{
    
    }
}
