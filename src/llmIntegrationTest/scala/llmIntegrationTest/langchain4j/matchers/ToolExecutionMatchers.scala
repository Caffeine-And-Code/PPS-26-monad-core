package llmIntegrationTest.langchain4j.matchers

import dev.langchain4j.service.Result
import llmIntegrationTest.langchain4j.matchers.IterableFormatter.formatForLogging
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.jdk.CollectionConverters.*

/** ScalaTest matchers for assertions on tool executions in LangChain4j results. */
object ToolExecutionMatchers:

  /**
   * Creates a matcher that requires exactly one execution of the named tool.
   *
   * @param expectedTool name of the tool to be executed
   * @return matcher that succeeds when the model executed only the provided tool
   */
  def onlyExecuteTool(expectedTool: String): Matcher[Result[?]] =
    Matcher { result =>
      val executedTools = result.getExecutedToolNameList

      MatchResult(
        executedTools == List(expectedTool),
        s"""Expected only tool "$expectedTool" to be executed, """ +
          s"but executed: ${executedTools.formatForLogging()}",
        s"""Only tool "$expectedTool" was executed"""
      )
    }

  /**
   * Creates a matcher that check that only the provided tools are called.
   *
   * @param expectedTools tool names expected to be called
   * @return matcher that succeeds when every expected tool name occurs in the result, and nothing else
   */
  def executeOnlyTheseTools(expectedTools: List[String]): Matcher[Result[?]] =
    Matcher { result =>
      val executedTools = result.getExecutedToolNameList

      val toolNotExecutedThatShouldHaveBeenExecuted = expectedTools.filterNot { toolExpected =>
        executedTools.contains(toolExpected)
      }
      val toolExecutedThatShouldHaveNotBeenExecuted = executedTools.filterNot { toolExpected =>
        expectedTools.contains(toolExpected)
      }

      MatchResult(
        toolNotExecutedThatShouldHaveBeenExecuted.isEmpty && toolNotExecutedThatShouldHaveBeenExecuted.isEmpty,
        s"""Expected only tool "${expectedTools.formatForLogging()}" to be executed, """ +
          s"but executed: ${executedTools.formatForLogging()}",
        s"""Only tool "${expectedTools.formatForLogging()}" was executed"""
      )
    }

  /**
   * Creates a matcher that rejects if there has been a tool execution.
   *
   * @return matcher that succeeds when the result contains no tool executions
   */
  def notExecuteTools: Matcher[Result[?]] =
    Matcher { result =>
      val executedTools = result.getExecutedToolNameList

      MatchResult(
        executedTools.isEmpty,
        s"""Expected no tool to be executed, """ +
          s"but executed: ${executedTools.formatForLogging()}",
        s"""No tools was executed"""
      )
    }

  extension (result: Result[?])

    private def getExecutedToolNameList: List[String] =
      result
        .toolExecutions()
        .asScala
        .map(_.request().name())
        .toList