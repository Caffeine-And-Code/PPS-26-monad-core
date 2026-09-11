package llmIntegrationTest.langchain4j.matchers

object IterableFormatter:

  extension (stringList: Iterable[String])

    def formatForLogging(): String =
      stringList.mkString("[\"", "\", \"", "\"]")
