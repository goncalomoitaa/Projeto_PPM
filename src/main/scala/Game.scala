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
import AtariGo.*
import javafx.geometry.{HPos, VPos}

class Game(gameState: GameState) extends Initializable{
    @FXML
    private var boardGrid : GridPane = _
    @FXML
    private var playerColor: Color = _
    @FXML
    private var botColor: Color = _

    @FXML
    override def initialize(url: URL, resourceBundle: ResourceBundle): Unit = {
        
    }
    
    @FXML
    def onGameBoardClick(event: MouseEvent): Unit = {
        val mouseX = event.getX
        val mouseY = event.getY

        val cols = boardGrid.getColumnConstraints.size()
        val rows = boardGrid.getRowConstraints.size()

        val cellWidth = boardGrid.getWidth / cols
        val cellHeight = boardGrid.getHeight / rows

        val col = (mouseX / cellWidth).toInt
        val row = (mouseY / cellHeight).toInt

        println(s"Click at col=$col, row=$row") // Debug output

        // Safety check for bounds
        if (col < 0 || col >= cols || row < 0 || row >= rows) return

        // Check if a circle is already in this cell
        val alreadyOccupied = boardGrid.getChildren.stream().anyMatch { node =>
            GridPane.getColumnIndex(node) == col &&
              GridPane.getRowIndex(node) == row &&
              node.isInstanceOf[Circle]
        }
        if (alreadyOccupied) {
            println("Cell already occupied")
            return
        }

        // Create the circle
        val circle = new Circle(35)
        circle.setFill(Color.RED)
        circle.setStrokeType(StrokeType.INSIDE)
        circle.setStroke(Color.BLACK)
        circle.setStrokeWidth(1)
        circle.setOpacity(1.0)

        // Center the circle in the grid cell
        GridPane.setColumnIndex(circle, col)
        GridPane.setRowIndex(circle, row)
        GridPane.setHalignment(circle, HPos.CENTER)
        GridPane.setValignment(circle, VPos.CENTER)

        boardGrid.getChildren.add(circle)
        println("Circle added")
    }


}
