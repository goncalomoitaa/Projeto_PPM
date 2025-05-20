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
      
    }
    
    @FXML
    def onStartGame(): Unit = {
        println("Button On Action triggered: game started!")
    }
}
