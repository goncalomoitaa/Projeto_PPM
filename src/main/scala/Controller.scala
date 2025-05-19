import javafx.fxml.{ FXML, Initializable }
import javafx.scene.control.{ Button, Label, TextField }
import javafx.scene.shape.Circle
import javafx.scene.input.{ MouseEvent, TouchEvent }
import javafx.scene.layout.GridPane
import javafx.scene.paint.Color

import java.net.URL
import java.util.ResourceBundle

class Controller extends Initializable{
//    @FXML
//    private var button1: Button = _
//    @FXML
//    private var textField1: TextField = _
//    @FXML
//    private var clickmebutton: Button = _
//    @FXML
//    private var label1: Label = _
    @FXML
    private var topLeftCircle : Circle = _
    private var boardGrid : GridPane = _
    
    override def initialize(url: URL, resourceBundle: ResourceBundle): Unit = {
        
        topLeftCircle.setFill(Color.WHITE)
        topLeftCircle.setOpacity(0.0)
//        GridPane.setRowIndex( topLeftCircle, 0 )
//        GridPane.setColumnIndex( topLeftCircle, 0 )
    }
    
    def OnMouseClickedCircle(event: MouseEvent): Unit = {
        topLeftCircle.setOpacity(1.0)
        println("Mouse clicked!")
        val currentRow = GridPane.getRowIndex( topLeftCircle )
        val currentCol = GridPane.getColumnIndex( topLeftCircle )
        
        //These may return null if the index was not set explicitly — you can default to 0:
//        val row = Option( GridPane.getRowIndex( topLeftCircle ) ).getOrElse( 0 )
//        val col = Option( GridPane.getColumnIndex( topLeftCircle ) ).getOrElse( 0 )
        
        println( s"Current position: row=$currentRow, column=$currentCol" )
        
        GridPane.setRowIndex( topLeftCircle, 4 )
        GridPane.setColumnIndex( topLeftCircle, 4 )
    }
    
//    def OnTouchCircle(event: TouchEvent): Unit = {  //   Does not get triggered, probably used for touch screens or pens?
//        println("Touch pressed!")
//    }
    
    def onMouseEnteredCircle(event: MouseEvent): Unit = {
        topLeftCircle.setOpacity(0.5)
        println("Mouse entered!")
    }
    
    def onMouseExitedCircle(event: MouseEvent): Unit = {
        topLeftCircle.setOpacity(0.0)
        println("Mouse exited!")
    }
    
//
//    def onButtonClickMeWasClicked(): Unit = {
//        println("Hello World")
//    }
//
//    def onButton1Clicked(): Unit = {
//        label1.setText("18cm")
//    }
//
//    def onMouseHoverButton1() : Unit ={
//        label1.setVisible(true)
//    }
//
//    def onMouseLeaveButton1() : Unit ={
//        label1.setVisible(false)
//    }
}
