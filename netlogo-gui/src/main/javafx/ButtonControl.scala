// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo(UTF8)

package org.nlogo.javafx

import javafx.event.ActionEvent
import javafx.fxml.{ FXML, FXMLLoader }
import javafx.scene.control.Label
import javafx.scene.image.ImageView
import javafx.scene.input.MouseEvent
import javafx.scene.layout.GridPane

import org.nlogo.core.{ Button => CoreButton }
import org.nlogo.internalapi.{ AddProcedureRun, CompiledButton => ApiCompiledButton, ModelAction, ModelRunner, ModelUpdate, RunComponent, RunnableModel }

class ButtonControl(compiledButton: ApiCompiledButton, runnableModel: RunnableModel, modelRunner: ModelRunner) extends GridPane with RunComponent {
  @FXML
  var foreverIcon: ImageView = _

  @FXML
  var label: Label = _

  @FXML
  def handleClickEvent(event: MouseEvent): Unit = {
    runnableModel.runTag(compiledButton.procedureTag, modelRunner)
    // compiledModel.runnableModel.submitAction(AddProcedureRun(compiledButton.procedureTag, true))
  }

  val button = compiledButton.widget

  locally {
    val loader = new FXMLLoader(getClass.getClassLoader.getResource("Button.fxml"))
    loader.setController(this)
    loader.setRoot(this)
    loader.load()
    label.setText(button.display orElse button.source getOrElse "")
    setPrefSize(button.right - button.left, button.bottom - button.top)
    if (button.forever) {
      foreverIcon.setPreserveRatio(true)
      val columnConstraint = getColumnConstraints.get(2)
      val rowConstraint     = getRowConstraints.get(2)
      //only bind on height because height is basically always less than width
      foreverIcon.setFitHeight(rowConstraint.getPrefHeight)
      foreverIcon.fitHeightProperty().bind(rowConstraint.prefHeightProperty())
      foreverIcon.setOpacity(1.0)
    }
  }

  def tagAction(action: ModelAction, actionTag: String): Unit = {}
  def updateReceived(update: ModelUpdate): Unit = {}
}
