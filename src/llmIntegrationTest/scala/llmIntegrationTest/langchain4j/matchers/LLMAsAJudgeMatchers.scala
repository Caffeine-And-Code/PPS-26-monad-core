package llmIntegrationTest.langchain4j.matchers

import dev.langchain4j.service.Result
import llmIntegrationTest.langchain4j.judge.Judgement.Pass
import llmIntegrationTest.langchain4j.judge.{Judgement, LlmJudgeAssistant}
import org.scalatest.matchers.{MatchResult, Matcher}

object LLMAsAJudgeMatchers:

  def beJudgedBy(judge: LlmJudgeAssistant): JudgeMatcherBuilder =
    JudgeMatcherBuilder(judge)

  final case class JudgeMatcherBuilder private[LLMAsAJudgeMatchers](
                                                                     judge: LlmJudgeAssistant
                                                                   ):

    infix def withCriteria(judgePrompt: String): Matcher[Result[String]] =
      Matcher { result =>
        val candidateResponse = result.content()
        val judgement = Judgement.from(judge.judge(judgePrompt, candidateResponse))
        val passed = judgement == Pass

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
