package monad_core.simulator.presentation.components.forms.base

import monad_core.engine.errors.EngineError
import monad_core.simulator.presentation.components.forms.base.FormDialog.matchToResult
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FormDialogTest extends AnyFunSuite with Matchers with MockFactory:

  test("if matchToResult is applied to an EngineError, the onError function is called"):
    val expectedError : EngineError = mock[EngineError]
    val either : Either[EngineError, Int] = Left(expectedError)
    val onError = mockFunction[EngineError, Unit]
    val onSuccess = mockFunction[Int, Unit]

    onError.expects(expectedError).once()
    onSuccess.expects(*).never()
    
    either.matchToResult(onError)(onSuccess)
    
  test("if matchToResult is applied to the desired result, the onRightResult function is called"):
    val expectedResult : Int = 10
    val either : Either[EngineError, Int] = Right(expectedResult)
    val onError = mockFunction[EngineError, Unit]
    val onSuccess = mockFunction[Int, Unit]

    onError.expects(*).never()
    onSuccess.expects(expectedResult).once()

    either.matchToResult(onError)(onSuccess)