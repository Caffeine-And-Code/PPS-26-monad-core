package monad_core.langchain4j.matchers

import dev.langchain4j.service.Result
import org.scalatest.matchers.{MatchResult, Matcher}

object AssistantResponseMatchers:

  def containsInResponse(information: String): Matcher[Result[String]] =
    containsInResponse(List(information))

  def containsInResponse(informations: Iterable[String]): Matcher[Result[String]] =
    Matcher { result =>
      val response = result.content()
      val normalizedResponse = response.toLowerCase()
      val missingInformation = informations.filterNot { information =>
        normalizedResponse.contains(information.toLowerCase())
      }.toList

      MatchResult(
        missingInformation.isEmpty,
        s"Expected response to contain ${informations.formatForLogging()}, " +
          s"but ${missingInformation.formatForLogging()} were missing from: \"$response\"",
        s"Response contained ${informations.formatForLogging()}"
      )
    }

  def notContainsInResponse(information: String): Matcher[Result[String]] =
    notContainsInResponse(List(information))

  def notContainsInResponse(informations: Iterable[String]): Matcher[Result[String]] =
    Matcher { result =>
      val response = result.content()
      val normalizedResponse = response.toLowerCase()
      val containedInformation = informations.filter { information =>
        normalizedResponse.contains(information.toLowerCase())
      }.toList

      MatchResult(
        containedInformation.isEmpty,
        s"Expected response to not contain ${informations.formatForLogging()}, " +
          s"but ${containedInformation.formatForLogging()} were present in: \"$response\"",
        s"Response not contained ${informations.formatForLogging()}"
      )
    }

  extension (stringList: Iterable[String]) {

    def formatForLogging(): String =
      stringList.mkString("[\"", "\", \"", "\"]")
  }