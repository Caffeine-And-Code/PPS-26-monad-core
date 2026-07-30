package monad_core.langchain4j.matchers

import dev.langchain4j.service.Result
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.jdk.CollectionConverters.*

object ToolExecutionMatchers:

  def onlyExecuteTool(expectedTool: String): Matcher[Result[?]] =
    Matcher {
      result =>
        val executedTools = result
          .getExecutedToolNameList

        MatchResult(
          executedTools == List(expectedTool),
          s"""Expected only tool "$expectedTool" to be executed, """ +
            s"but executed: ${executedTools.formatForLogging()}",
          s"""Only tool "$expectedTool" was executed"""
        )
    }

  def executeOnlyTheseTools(expectedTools: List[String]): Matcher[Result[?]] =
    Matcher {
      result =>
        val executedTools = result
          .getExecutedToolNameList

        MatchResult(
          executedTools == expectedTools,
          s"""Expected only tool "${expectedTools.formatForLogging()}" to be executed, """ +
            s"but executed: ${executedTools.formatForLogging()}",
          s"""Only tool "${expectedTools.formatForLogging()}" was executed"""
        )
    }

  def notExecuteTools: Matcher[Result[?]] =
    Matcher {
      result =>
        val executedTools = result.getExecutedToolNameList

        MatchResult(
          executedTools.isEmpty,
          s"""Expected no tool to be executed, """ +
            s"but executed: ${executedTools.formatForLogging()}",
          s"""No tools was executed"""
        )
    }

  extension (result: Result[?]){

    def getExecutedToolNameList: List[String] =
      result
        .toolExecutions()
        .asScala
        .map(_.request().name())
        .toList
  }

  extension (stringList: List[String]){

    def formatForLogging(): String =
      stringList.mkString("[", ", ", "]")
  }