import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.shape.Circle
import javafx.scene.input.{ MouseEvent, TouchEvent }

class Controller {
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
    
    def OnMouseClickedCircle(event: MouseEvent): Unit = {
        println("Mouse clicked!")
    }
    
    def OnTouchCircle(event: TouchEvent): Unit = {
        println("Touch pressed!")
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
