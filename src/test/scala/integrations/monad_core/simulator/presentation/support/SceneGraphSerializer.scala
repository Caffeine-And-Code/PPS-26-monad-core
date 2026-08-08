package integrations.monad_core.simulator.presentation.support

import javafx.scene.control.Labeled
import javafx.scene.{Node, Parent}

import scala.jdk.CollectionConverters.*

object SceneGraphSerializer:

  case class NodeSnapshot(
                           nodeType: String,
                           id: Option[String],
                           styleClass: List[String],
                           text: Option[String],
                           children: List[NodeSnapshot]
                         )
  
  def snapshotOf(node: Node): NodeSnapshot =
    val childrenList = node match
      case parent: Parent => parent.getChildrenUnmodifiable.asScala.map(snapshotOf).toList
      case _ => Nil

    val textVal = node match
      case labeled: Labeled => Option(labeled.getText).filter(_.nonEmpty)
      case _ => None

    NodeSnapshot(
      nodeType = node.getClass.getSimpleName,
      id = Option(node.getId).filter(_.nonEmpty),
      styleClass = node.getStyleClass.asScala.toList.filter(_.nonEmpty),
      text = textVal,
      children = childrenList
    )

  def toJson(snapshot: NodeSnapshot, indentLevel: Int = 0): String =
    val pad = "  " * indentLevel
    val childPad = "  " * (indentLevel + 1)

    val idStr = snapshot.id.map(id => s""""id": "$id"""")
    val textStr = snapshot.text.map(t => s""""text": "${t.replace("\n", "\\n").replace("\"", "\\\"")}"""")
    val stylesStr =
      if snapshot.styleClass.nonEmpty then
        Some(s""""styleClass": [${snapshot.styleClass.map(s => s"\"$s\"").mkString(", ")}]""")
      else None

    val childrenStr =
      if snapshot.children.nonEmpty then
        val serializedChildren = snapshot.children.map(c => toJson(c, indentLevel + 2)).mkString(",\n")
        Some(s""""children": [\n$serializedChildren\n$childPad]""")
      else None

    val fields = List(
      Some(s""""type": "${snapshot.nodeType}""""),
      idStr,
      stylesStr,
      textStr,
      childrenStr
    ).flatten

    s"$pad{\n$childPad" + fields.mkString(s",\n$childPad") + s"\n$pad}"