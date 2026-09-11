package monad_core.simulator.presentation.panels.support

/**
 * Provides base styles for different types of Panels.
 */
object PanelStyles:

  /**
   * Return the ScalaFx CSS to render a generic panel that stick to the Gui application base style.
   *
   * @return String that can be applied to a Node CSS
   */
  def base: String =
    "-fx-background-color: #26282c; -fx-background-radius: 10; -fx-border-radius: 10; -fx-padding: 10px;"

  /**
   * Return the ScalaFx CSS to render the [[SceneRendererPanel]] panel.
   *
   * @return String that can be applied to a Node CSS
   */
  def sceneRenderer: String =
    base + "-fx-background-color: #454951, #26282c; -fx-background-insets: 0, 10px;"
