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
import TUI_Utils._
import AtariGo._

class Game(gameState: GameState) extends Initializable{
    @FXML
    private var boardGrid : GridPane = _
    private var playerColor: Color = _
    private var botColor: Color = _

    @FXML
    override def initialize(url: URL, resourceBundle: ResourceBundle): Unit = {
        
    }
    
    @FXML
    def onGameBoardClick(event: MouseEvent): Unit = {
        val mouseX = event.getX
        val mouseY = event.getY
        val circleToAdd: Circle = new Circle(35)
        circleToAdd.setFill(playerColor)
        
        val cols = boardGrid.getColumnConstraints.size()
        val rows = boardGrid.getRowConstraints.size()
        
        val cellWidth = boardGrid.getWidth / cols
        val cellHeight = boardGrid.getHeight / rows
        
        val col = ( mouseX / cellWidth ).toInt
        val row = ( mouseY / cellHeight ).toInt

    }
}
