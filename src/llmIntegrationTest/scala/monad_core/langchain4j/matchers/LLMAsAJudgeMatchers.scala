package monad_core.langchain4j.matchers

import dev.langchain4j.service.Result
import monad_core.langchain4j.judge.LlmJudgeAssistant
import org.scalatest.matchers.{MatchResult, Matcher}

import java.util.Locale

object LLMAsAJudgeMatchers:

  def beJudgedBy(judge: LlmJudgeAssistant): JudgeMatcherBuilder =
    JudgeMatcherBuilder(judge)

  final case class JudgeMatcherBuilder private[LLMAsAJudgeMatchers] (
      judge: LlmJudgeAssistant
  ):

    infix def withCriteria(judgePrompt: String): Matcher[Result[String]] =
      Matcher { result =>
        val candidateResponse = result.content()
        val judgement = judge.judge(judgePrompt, candidateResponse).trim
        val passed = judgement.toUpperCase(Locale.ROOT) == "PASS"

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
