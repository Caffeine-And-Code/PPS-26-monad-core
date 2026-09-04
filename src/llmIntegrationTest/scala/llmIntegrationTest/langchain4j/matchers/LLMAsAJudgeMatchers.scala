package llmIntegrationTest.langchain4j.matchers

import dev.langchain4j.service.Result
import llmIntegrationTest.langchain4j.judge.Judgement.Pass
import llmIntegrationTest.langchain4j.judge.{Judgement, LlmJudgeAssistant}
import org.scalatest.Assertion
import org.scalatest.Assertions.assert
import org.scalatest.matchers.{MatchResult, Matcher}

/**
 * ScalaTest syntax for evaluating assistant responses through an [[LlmJudgeAssistant]]
 */
object LLMAsAJudgeMatchers:

  private def beJudgedBy(judge: LlmJudgeAssistant): JudgeMatcherBuilder =
    JudgeMatcherBuilder(judge)

  extension (result: Result[String])

    /**
     * Associates an assistant result with the judge that will evaluate it.
     *
     * @param judge judge service used for the evaluation
     * @return intermediate value exposing the `shouldSatisfy` assertion syntax
     */
    infix def judgedBy(judge: LlmJudgeAssistant): JudgedResult =
      JudgedResult(result, judge)

  /**
   * Assistant result associated with the LLM judge selected for the judgment.
   *
   * @param result candidate model result
   * @param judge judge service used for the evaluation
   */
  final case class JudgedResult private[LLMAsAJudgeMatchers] (
      result: Result[String],
      judge: LlmJudgeAssistant
  ):

    /**
     * Asserts that the candidate response satisfies a textual criterion according to the judge.
     *
     * @param judgePrompt criterion supplied to the judge
     * @return successful ScalaTest assertion when the judge returns `PASS`
     */
    infix def shouldSatisfy(judgePrompt: String): Assertion =
      val judgement = beJudgedBy(judge).withCriteria(judgePrompt)(result)
      assert(judgement.matches, judgement.failureMessage)

  /**
   * Builder for having an evaluation of the result of a model judged by a judge
   *
   * @param judge judge service
   */
  final case class JudgeMatcherBuilder private[LLMAsAJudgeMatchers] (
      judge: LlmJudgeAssistant
  ):

    /**
     * Creates a matcher that evaluate a result with the judge criteria provided
     *
     * @param judgePrompt criterion that the assistant response must satisfy
     * @return matcher that succeeds when the judge output is [[Judgement.Pass]]
     */
    infix def withCriteria(judgePrompt: String): Matcher[Result[String]] =
      Matcher { result =>
        val candidateResponse = result.content()
        val judgement         = Judgement.from(judge.judge(judgePrompt, candidateResponse))
        val passed            = judgement == Pass

        MatchResult(
          passed,
          s"""The judge rejected the response.
             Criterion: $judgePrompt
             Response: $candidateResponse
             Judge output: $judgement""".stripMargin,
          s"""The judge accepted the response.
             Criterion: $judgePrompt
             Judge output: $judgement""".stripMargin
        )
      }
