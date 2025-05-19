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

class Menu extends Initializable {
    @FXML
    private var boardGrid : GridPane = _
    private var playerColor: Color = _
    private var botColor: Color = _
    private var gameState: GameState = _

    @FXML
    override def initialize(url: URL, resourceBundle: ResourceBundle): Unit = {
      
    }
}
