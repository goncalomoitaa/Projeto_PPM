import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, TextField}

class Controller {
    @FXML
    private var button1: Button = _
    @FXML
    private var textField1: TextField = _
    @FXML
    private var clickmebutton: Button = _
    @FXML
    private var label1: Label = _
    
    
    def onButtonClickMeWasClicked(): Unit = {
        println("Hello World")
    }

    def onButton1Clicked(): Unit = {
        label1.setText("18cm")
    }

    def onMouseHoverButton1() : Unit ={
        label1.setVisible(true)
    }

    def onMouseLeaveButton1() : Unit ={
        label1.setVisible(false)
    }
}
