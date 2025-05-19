import javafx.fxml.{ FXML, Initializable }
import javafx.scene.control.{ Button, Label, TextField }
import javafx.scene.shape.Circle
import javafx.scene.input.{ MouseEvent, TouchEvent }
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
    
    override def initialize(url: URL, resourceBundle: ResourceBundle): Unit = {
        
        topLeftCircle.setFill(Color.WHITE)
        topLeftCircle.setOpacity(0.0)
    }
    
    def OnMouseClickedCircle(event: MouseEvent): Unit = {
        topLeftCircle.setOpacity(1.0)
        println("Mouse clicked!")
    }
    
    def OnTouchCircle(event: TouchEvent): Unit = {
        println("Touch pressed!")
    }
    
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
