package llmIntegrationTest.langchain4j.matchers

import dev.langchain4j.service.Result
import llmIntegrationTest.langchain4j.matchers.IterableFormatter.formatForLogging
import org.scalatest.matchers.{MatchResult, Matcher}

/** ScalaTest matchers for assertions on the textual content of LangChain4j results. */
object AssistantResponseMatchers:

  /**
   * Creates a case-insensitive matcher for one required information.
   *
   * @param information text expected in the assistant response
   * @return matcher that succeeds when the response contains `information`
   */
  def containsInResponse(information: String): Matcher[Result[String]] =
    containsInResponse(List(information))

  /**
   * Creates a case-insensitive matcher for multiple required information.
   *
   * @param informations text expected in the assistant response
   * @return matcher that succeeds when the response contains every supplied texts
   */
  def containsInResponse(informations: Iterable[String]): Matcher[Result[String]] =
    Matcher { result =>
      val response           = result.content()
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

  /**
   * Creates a case-insensitive matcher for one forbidden text.
   *
   * @param information text that must be absent from the assistant response
   * @return matcher that succeeds when the response does not contain `information`
   */
  def notContainsInResponse(information: String): Matcher[Result[String]] =
    notContainsInResponse(List(information))

  /**
   * Creates a case-insensitive matcher for multiple forbidden texts.
   *
   * @param informations texts that must be absent from the assistant response
   * @return matcher that succeeds when the response contains none of the supplied texts
   */
  def notContainsInResponse(informations: Iterable[String]): Matcher[Result[String]] =
    Matcher { result =>
      val response           = result.content()
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
