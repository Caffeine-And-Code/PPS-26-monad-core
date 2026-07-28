package integrations.monad_core.simulator.presentation.support

import javafx.scene.Parent
import javafx.scene.control.{Button, ComboBox, TextField}
import org.scalatest.funsuite.AnyFunSuite
import scalafx.scene.control.ListView

import scala.jdk.CollectionConverters.IterableHasAsScala

trait FormTesting extends AnyFunSuite with ScalaFxInit with SnapshotTesting:

  private def getActiveStageRoot: Parent =
    val activeStage = getRequiredActiveStage
    activeStage.getScene.getRoot

  def allFormFields: List[TextField] =
    getActiveStageRoot.lookupAll(".form-field-input")
      .asScala
      .collect { case tf: javafx.scene.control.TextField => tf }
      .toList
    
  def allFormComboBoxes: List[ComboBox[String]] =
    getActiveStageRoot.lookupAll(".form-field-select")
      .asScala
      .collect { case cb: javafx.scene.control.ComboBox[String] @unchecked => cb }
      .toList

  def allFormMultiSelects: List[ListView[String]] =
    getActiveStageRoot.lookupAll(".form-field-multiselect")
      .asScala
      .collect { case multi: javafx.scene.control.ListView[String] @unchecked => new ListView[String](multi) }
      .toList 
      
  def formSaveButton: Button =
    getActiveStageRoot.lookup(".form-dialog-save")
      .asInstanceOf[javafx.scene.control.Button]